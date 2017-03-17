package testchipip

import chisel3.util.ShiftRegister
import chisel3.util.Enum
import chisel3.util.Counter
import util.AsyncResetReg
import chisel3._
import chisel3.util._
import uncore.tilelink._
import cde.Parameters
import chisel3.core.ExplicitCompileOptions.NotStrict

class ResetSync(c: Clock, lat: Int = 2) extends Module(_clock = c) {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val reset_sync = Output(Bool())
  })
  io.reset_sync := ShiftRegister(io.reset,lat)
}

object ResetSync {
  def apply(r: Bool, c: Clock): Bool = {
    val sync = Module(new ResetSync(c,2))
    sync.suggestName("resetSyncInst")
    sync.io.reset := r
    sync.io.reset_sync
  }
}

// a counter that clock gates most of its MSBs using the LSB carry-out
// uses asyncresetregs to make it easy for cross-clock domain work
case class AsyncWideCounter(width: Int, inc: UInt = 1.U, reset: Boolean = true)
{
  private val isWide = width > 2*inc.getWidth
  private val smallWidth = if (isWide) inc.getWidth max log2Up(width) else width
  private val widerNextSmall = Wire(UInt((smallWidth + 1).W))
  private val nextSmall = Wire(UInt(smallWidth.W))
  private val small = if (reset) AsyncResetReg(nextSmall, 0, "smallReg") else AsyncResetReg(nextSmall, "smallReg")
  widerNextSmall := small +& inc
  nextSmall := widerNextSmall

  private val large = if (isWide) {
    val nextR = Wire(UInt((width - smallWidth).W))
    val r = if (reset) AsyncResetReg(nextR, 0, "rReg") else AsyncResetReg(nextR, "rReg")
    when (widerNextSmall(smallWidth)) {
      nextR := r +& 1.U
    }.otherwise {
      nextR := r
    }
    r
  } else null

  val value = if (isWide) large ## small else small
  lazy val carryOut = {
    val lo = (small ^ widerNextSmall) >> 1
    if (!isWide) lo else {
      val hi = Mux(widerNextSmall(smallWidth), large ^ (large +& 1.U), 0.U) >> 1
      hi ## lo
    }
  }
}

// As WideCounter, but it's a module so it can take arbitrary clocks
class WideCounterModule(w: Int, inc: UInt = 1.U, reset: Boolean = true, clockSignal: Clock = null, resetSignal: Bool = null)
    extends Module(Option(clockSignal), Option(resetSignal)) {
  val io = IO(new Bundle {
    val value = Output(UInt(w.W))
  })
  io.value := AsyncWideCounter(w, inc, reset).value
}

object WideCounterModule {
  def apply(w: Int, c: Clock, r: Bool) = {
    val counter = Module(new WideCounterModule(w, clockSignal = c, resetSignal = r))
    counter.suggestName("wideCounterInst")
    counter.io.value
  }
  def apply(w: Int, c: Clock) = {
    val counter = Module(new WideCounterModule(w, clockSignal = c))
    counter.suggestName("wideCounterInst")
    counter.io.value
  }
}

/**
 * Simple widget to apply a sequence of puts to a port
 * @param s a Seq[Tuple2[BigInt,Int]] of address/data pairs
 */
class PutSeqDriver(val s: Seq[Tuple2[BigInt,Int]])(implicit p: Parameters) extends Driver()(p) {

  val (s_idle :: s_put_req :: s_put_resp :: s_done :: Nil) = Enum(4)
  val state = Reg(init = s_idle)

  val n = s.size
  val puts = Vec(s.map { case (a, d) =>
    val addr = a.U
    val beatAddr = addr(tlBeatAddrBits+tlByteAddrBits-1,tlByteAddrBits)
    val blockAddr = addr(tlBlockAddrBits+tlBeatAddrBits+tlByteAddrBits-1,tlBeatAddrBits+tlByteAddrBits)
    Put(0.U, blockAddr, beatAddr, d.U)
  })

  val (put_cnt, put_done) = Counter(state === s_put_resp && io.mem.grant.valid, n)

  io.mem.acquire.valid := state === s_put_req
  io.mem.acquire.bits := puts(put_cnt)
  io.mem.grant.ready := state === s_put_resp

  when (state === s_idle && io.start) { state := s_put_req }
  when (state === s_put_req && io.mem.acquire.ready) { state := s_put_resp }
  when (state === s_put_resp && io.mem.grant.valid) {
    state := Mux(put_done, s_done, s_put_req)
  }

  io.finished := (state === s_done)

}

/**
 * Simple widget to apply a sequence of gets to a port and check that the responses are correct
 * @param s a Seq[Tuple2[BigInt,Int]] of address/data pairs
 */
class GetSeqChecker(val s: Seq[Tuple2[BigInt,Int]])(implicit p: Parameters) extends Driver()(p) {

  val (s_idle :: s_get_req :: s_get_resp :: s_done :: Nil) = Enum(4)
  val state = Reg(init = s_idle)

  val n = s.size
  val gets = Vec(s.map { case (a, d) =>
    val addr = a.U
    val beatAddr = addr(tlBeatAddrBits+tlByteAddrBits-1,tlByteAddrBits)
    val blockAddr = addr(tlBlockAddrBits+tlBeatAddrBits+tlByteAddrBits-1,tlBeatAddrBits+tlByteAddrBits)
    Get(0.U, blockAddr, beatAddr)
  })

  val (get_cnt, get_done) = Counter(state === s_get_resp && io.mem.grant.valid, n)

  val data = Vec(s.map { _._2.U })

  when (io.mem.grant.fire()) {
    assert(data(get_cnt) === io.mem.grant.bits.data)
  }

  io.mem.acquire.valid := state === s_get_req
  io.mem.acquire.bits := gets(get_cnt)
  io.mem.grant.ready := state === s_get_resp

  when (state === s_idle && io.start) { state := s_get_req }
  when (state === s_get_req && io.mem.acquire.ready) { state := s_get_resp }
  when (state === s_get_resp && io.mem.grant.valid) {
    state := Mux(get_done, s_done, s_get_req)
  }

  io.finished := (state === s_done)

}

// Use gray coding to safely synchronize a word across a clock crossing.
// This should be placed in the receiver's clock domain.
class WordSync[T <: Data](gen: T, lat: Int = 2) extends Module {
  val size = gen.getWidth
  val io = IO(new Bundle {
    val in = gen.chiselCloneType.flip
    val out = gen.chiselCloneType
    val tx_clock = Clock(INPUT)
  })
  val bin2gray = Module(new BinToGray(gen,io.tx_clock))
  bin2gray.io.bin := io.in
  val out_gray = ShiftRegister(bin2gray.io.gray, lat)
  io.out := gen.cloneType.fromBits((0 until size).map{ out_gray.asUInt >> _.U }.reduceLeft(_^_))
}

class BinToGray[T <: Data](gen: T, c: Clock) extends Module(_clock = c) {
  val io = IO(new Bundle {
    val bin = gen.chiselCloneType.flip
    val gray = UInt(gen.getWidth.W)
  })
  io.gray := Reg(next=(io.bin.asUInt ^ (io.bin.asUInt >> 1.U)))
}

object WordSync {
  def apply[T <: Data](word: T, c: Clock) = {
    val sync = Module(new WordSync(word))
    sync.suggestName("wordSyncInst")
    sync.io.tx_clock := c
    sync.io.in := word
    sync.io.out
  }
  def apply[T <: Data](gen: T, word: Data, c: Clock, lat: Int = 2) = {
    val sync = Module(new WordSync(gen,lat))
    sync.suggestName("wordSyncInst")
    sync.io.tx_clock := c
    sync.io.in := word
    sync.io.out
  }
}
