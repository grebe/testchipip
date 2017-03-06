package testchipip

import chisel3._
import unittest.UnitTests
import rocketchip.{BaseConfig, NCoreplexExtClients, ExtMemSize}
import coreplex.NMemoryChannels
import uncore.tilelink.{TLId, TLKey}
import cde.{Parameters, Config, CDEMatchError}

class WithTestChipUnitTests extends Config(
  (pname, site, here) => pname match {
    case NCoreplexExtClients => 0
    case UnitTests => (testParams: Parameters) =>
      TestChipUnitTests(testParams)
    case TLId => "L1toL2"
    case _ => throw new CDEMatchError
  })

class TestChipUnitTestConfig extends Config(
  new WithTestChipUnitTests ++ new BaseConfig)

class WithSerialAdapter extends Config(
  (pname, site, here) => pname match {
    case SerialInterfaceWidth => 32
    case _ => throw new CDEMatchError
  })

class WithSRAM(nBanksPerChannel: Int) extends Config(
  (pname, site, here) => pname match {
    case NSRAMBanksPerChannel => nBanksPerChannel
    case NSRAMBlocksPerBank => {
      val blockBytes = site(TLKey("L2toMC")).dataBits / 8
      val nBanks = nBanksPerChannel * site(NMemoryChannels)
      (site(ExtMemSize) / (nBanks * blockBytes)).toInt
    }
    case _ => throw new CDEMatchError
  })
