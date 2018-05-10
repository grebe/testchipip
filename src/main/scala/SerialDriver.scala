package testchipip

import chisel3._
import chisel3.util._
import junctions._

abstract class SerialDriver(w: Int) extends Module {
  val io = IO(new Bundle {
    val serial = Flipped(new SerialIO(w))
    val exit = Output(Bool())
  })
}

class SimSerial extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val serial = Flipped(new SerialIO(32))
    val exit = Output(Bool())
  })
}

class SimSerialWrapper(w: Int) extends SerialDriver(w) {
  val bbox = Module(new SimSerial)
  bbox.io.clock := clock
  bbox.io.reset := reset
  SerialWidthAdapter(io.serial, bbox.io.serial)
  io.exit := bbox.io.exit
}


class SerializerWidthAdapter(in_w: Int, out_w: Int) extends Module {
  require(in_w > out_w)
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(in_w.W)))
    val out = Decoupled(UInt(out_w.W))
  })

  val dataBits = in_w
  val dataBeats = (dataBits - 1) / out_w + 1
  val data = Reg(UInt(dataBits.W))

  val sending = RegInit(false.B)
  val (sendCount, sendDone) = Counter(io.out.fire(), dataBeats)

  io.in.ready := !sending
  io.out.valid := sending
  io.out.bits := data(out_w-1, 0)

  when (io.in.fire()) {
    data := io.in.bits
    sending := true.B
  }

  when (io.out.fire()) { data := data >> out_w.U }

  when (sendDone) { sending := false.B }
}

class DeserializerWidthAdapter(in_w: Int, out_w: Int) extends Module {
  require(in_w < out_w)
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(in_w.W)))
    val out = Decoupled(UInt(out_w.W))
  })

  val dataBits = out_w
  val dataBeats = (dataBits - 1) / in_w + 1
  val data = Reg(Vec(dataBeats, UInt(in_w.W)))

  val receiving = RegInit(true.B)
  val (recvCount, recvDone) = Counter(io.in.fire(), dataBeats)

  io.in.ready := receiving
  io.out.valid := !receiving
  io.out.bits := data.asUInt

  when (io.in.fire()) {
    data(recvCount) := io.in.bits
  }

  when (recvDone) { receiving := false.B }

  when (io.out.fire()) { receiving := true.B }
}

class SerialModule(w: Int) extends Module {
  val io = IO(new SerialIO(w))
}

class SerialWidener(in_w: Int, out_w: Int) extends Module {
  require(in_w < out_w)
  val io = IO(new Bundle {
    val in = Flipped(new SerialIO(in_w))
    val out = new SerialIO(out_w)
  })

  val ser = Module(new SerializerWidthAdapter(out_w, in_w))
  ser.io.in <> io.out.in
  io.in.in <> ser.io.out
  val des = Module(new DeserializerWidthAdapter(in_w, out_w))
  des.io.in <> io.in.out
  io.out.out <> des.io.out
}

class SerialNarrower(in_w: Int, out_w: Int) extends Module {
  require(in_w > out_w)
  val io = IO(new Bundle {
    val in = Flipped(new SerialIO(in_w))
    val out = new SerialIO(out_w)
  })

  val ser = Module(new SerializerWidthAdapter(in_w, out_w))
  ser.io.in <> io.in.out
  io.out.out <> ser.io.out
  val des = Module(new DeserializerWidthAdapter(out_w, in_w))
  des.io.in <> io.out.in
  io.in.in <> des.io.out
}

object SerialWidthAdapter {
  def apply(in: SerialIO, out_w: Int): SerialIO = {
    val in_w = in.in.bits.getWidth

    if (out_w > in_w) {
      val widener = Module(new SerialWidener(in_w, out_w))
      widener.io.in <> in
      widener.io.out
    } else if (out_w < in_w) {
      val narrower = Module(new SerialNarrower(in_w, out_w))
      narrower.io.in <> in
      narrower.io.out
    } else { in }
  }
  def apply(in: SerialIO, out: SerialIO): Unit = {
    out <> apply(in, out.in.bits.getWidth)
  }
}
