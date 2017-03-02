package testchipip

import chisel3._
import chisel3.util._
import uncore.tilelink._
import cde.{Parameters, Field}

case object NSRAMBanksPerChannel extends Field[Int]
case object NSRAMBlocksPerBank extends Field[Int]

class MainSRAMBank(implicit p: Parameters) extends TLModule()(p) {
  val io = IO(Flipped(new ClientUncachedTileLinkIO))

  val s_idle :: s_read :: s_wait_read :: s_write :: s_resp_write :: Nil = Enum(5)
  val state = Reg(init = s_idle)

  val nBlocks = p(NSRAMBlocksPerBank)
  val mem = SeqMem(nBlocks * tlDataBeats, UInt(tlDataBits.W))

  val ren = state === s_read
  val rblock = Reg(UInt(tlBlockAddrBits.W))
  val (rbeat, rdone) = Counter(ren, tlDataBeats)
  val wen = io.acquire.fire() && io.acquire.bits.hasData()
  val rdata = mem.read(Cat(rblock, rbeat), ren && !wen)

  val r_rbeat = Reg(next = rbeat)
  val r_ren = Reg(next = ren)

  val buffer = Reg(Vec(tlDataBeats, UInt(tlDataBits.W)))
  val buffer_valid = Reg(init = 0.U(tlDataBeats.W))

  val r_acq = Reg(new AcquireMetadata)

  val multibeat_gnt = !r_acq.hasData()
  val multibeat_fire = multibeat_gnt && io.grant.fire()
  val (gnt_beat, gnt_done) = Counter(multibeat_fire, tlDataBeats)

  when (r_ren) { buffer(r_rbeat) := rdata }
  buffer_valid := (buffer_valid |
    Mux(r_ren, UIntToOH(r_rbeat), 0.U)) &
    ~Mux(multibeat_fire, UIntToOH(gnt_beat), 0.U)

  val wdata = io.acquire.bits.data
  val wblock = io.acquire.bits.addr_block
  val wbeat = io.acquire.bits.addr_beat

  when (wen) { mem.write(Cat(wblock, wbeat), wdata) }

  when (state === s_idle && io.acquire.valid) {
    val acq = io.acquire.bits
    r_acq := acq
    when (acq.isBuiltInType(Acquire.getBlockType)) {
      rblock := acq.addr_block
      buffer_valid := 0.U
      state := s_read
    } .elsewhen (acq.isBuiltInType(Acquire.putBlockType)) {
      state := s_write
    } .otherwise {
      assert(false.B, "Unexpected TL operation")
    }
  }

  when (state === s_write && io.acquire.valid && io.acquire.bits.last()) {
    state := s_resp_write
  }

  when (rdone) { state := s_wait_read }
  when (io.grant.fire() && io.grant.bits.last()) { state := s_idle }

  io.acquire.ready := (state === s_idle) || (state === s_write)
  io.grant.valid := Mux(multibeat_gnt,
    (state =/= s_idle) && (buffer_valid >> gnt_beat)(0),
    state === s_resp_write)
  io.grant.bits := Grant(
    is_builtin_type = true.B,
    g_type = r_acq.getBuiltInGrantType(),
    client_xact_id = r_acq.client_xact_id,
    manager_xact_id = 0.U,
    addr_beat = gnt_beat,
    data = buffer(gnt_beat))

  assert(!io.acquire.valid || !io.acquire.bits.hasData() ||
         !io.acquire.bits.hasPartialWritemask(),
         "MainSRAM does not accept partial writes")
}

class MainSRAMChannel(implicit p: Parameters) extends TLModule()(p) {
  val io = IO(Flipped(new ClientUncachedTileLinkIO))

  val nBanks = p(NSRAMBanksPerChannel)
  val bankIdBits = log2Up(nBanks)

  def cutTileLinkAddr(in: ClientUncachedTileLinkIO): ClientUncachedTileLinkIO = {
    val out = Wire(new ClientUncachedTileLinkIO)
    out.acquire.valid := in.acquire.valid
    out.acquire.bits := in.acquire.bits
    out.acquire.bits.addr_block := in.acquire.bits.addr_block >> bankIdBits.U
    in.acquire.ready := out.acquire.ready
    in.grant <> out.grant
    out
  }

  def routeSel(addr: UInt): UInt = {
    val blockOffset = tlBeatAddrBits + tlByteAddrBits
    val bankId = addr(bankIdBits + blockOffset - 1, blockOffset)
    UIntToOH(bankId)
  }

  val banks = Seq.fill(nBanks) { Module(new MainSRAMBank) }
  if (nBanks == 1) {
    banks.head.io <> io
  } else {
    val router = Module(new ClientUncachedTileLinkIORouter(nBanks, routeSel _))
    router.io.in <> io
    banks.zip(router.io.out).foreach {
      case (bank, out) => bank.io <> cutTileLinkAddr(out)
    }
  }
}
