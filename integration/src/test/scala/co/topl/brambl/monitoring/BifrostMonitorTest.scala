package co.topl.brambl.monitoring

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.implicits.toTraverseOps
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.builders.locks.LockTemplate.PredicateTemplate
import co.topl.brambl.builders.locks.PropositionTemplate.HeightTemplate
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances.ioTransactionSignable
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.{BifrostQueryAlgebra, GenusQueryAlgebra, RpcChannelResource}
import co.topl.brambl.models.box.Attestation
import co.topl.brambl.monitoring.BifrostMonitor.AppliedBifrostBlock
import co.topl.brambl.syntax.{LvlType, ioTransactionAsTransactionSyntaxOps}
import co.topl.quivr.api.Prover
import io.grpc.ManagedChannel

import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}

class BifrostMonitorTest extends munit.CatsEffectSuite {

  override val munitTimeout: FiniteDuration = Duration(180, "s")

  val channelResource1: Resource[IO, ManagedChannel] =
    RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)
  val channelResource2: Resource[IO, ManagedChannel] =
    RpcChannelResource.channelResource[IO]("localhost", 9086, secureConnection = false)
  val bifrostQuery1: BifrostQueryAlgebra[IO] = BifrostQueryAlgebra.make[IO](channelResource1)
  val bifrostQuery2: BifrostQueryAlgebra[IO] = BifrostQueryAlgebra.make[IO](channelResource2)



  test("Monitor blocks with a reorg") {
    assertIO(for {
      _ <- IO.println("node 1 tip:")
      node1Tip <- bifrostQuery1.blockByDepth(0)
      _ <- IO.println(node1Tip)
      _ <- IO.println("node 2 tip:")
      node2Tip <- bifrostQuery2.blockByDepth(0)
      _ <- IO.println(node2Tip)
      _ <- IO.println("disconnecting node 2 from network")
      _ <- disconnectBifrostNodes("bridge", "bifrost02").start.allocated // The 2 nodes start disconnected
      _ <- IO.println("starting monitor service")
      monitor <- BifrostMonitor(bifrostQuery1) // monitoring node 1
      blockStream = monitor.monitorBlocks().through(s => s.map(r => {
        println(r)
        r
      }))
      _ <- IO.println("node 1 making 1 block")
      node1MintBlocks <- bifrostQuery1.makeBlock(1) // monitor should report this 1 block
      _ <- IO.println("node 2 making 2 blocks")
      node2MintBlocks <- bifrostQuery2.makeBlock(2)

      // connect blocks. the monitor should unapply the 1 block, and then apply the 2 new blocks
      _ <- IO.println("reconnecting node 2 from network")
      _ <- connectBifrostNodes("bridge", "bifrost02").start.allocated

      _ <- IO.println("node 2 making 1 block").andWait(5.seconds)
      additionalMintBlocks <- bifrostQuery2.makeBlock(1) // monitor should report this

      _ <- IO.println("compiling stream")
      blocks <- blockStream.interruptAfter(10.seconds).compile.toList
    } yield {
      println(s"blocks:  $blocks")
      blocks.length == 5
    } // applied, unapplied, applied, applied, applied
      , true )
  }

  /**

   --- works

   new version, no staker arguments (works):
   docker run --rm -d -p 9085:9085 -p 9084:9084 -p 9091:9091 ghcr.io/topl/bifrost-node:2.0.0-beta3-24-7fd725a9

   old version, no staker arguments (works):
   docker run --rm -d -p 9085:9085 -p 9084:9084 -p 9091:9091 ghcr.io/topl/bifrost-node:2.0.0-beta3

   --- does not work (TimeoutException)

   old version, with staker arguments; config.yaml does not contain line "regtest-enabled" (does not work):
   docker run --rm -d -p 9085:9085 -p 9084:9084 -p 9091:9091 -v /home/diadem/node01:/bifrost-staking:rw ghcr.io/topl/bifrost-node:2.0.0-beta3 -- --config /bifrost-staking/config.yaml

   new version, with staker arguments; not regtest mode & config.yaml does not contain line "regtest-enabled" (does not work):
   docker run --rm -d -p 9085:9085 -p 9084:9084 -p 9091:9091 -v /home/diadem/node01:/bifrost-staking:rw ghcr.io/topl/bifrost-node:2.0.0-beta3-24-7fd725a9 -- --config /bifrost-staking/config.yaml

   new version, with staker arguments and regtest mode (config.yaml contains "regtest-enabled: true") (does not work):
   docker run --rm -d -p 9085:9085 -p 9084:9084 -p 9091:9091 -v /home/diadem/node01:/bifrost-staking:rw ghcr.io/topl/bifrost-node:2.0.0-beta3-24-7fd725a9 -- --config /bifrost-staking/config.yaml --regtest

   new version, no staker arguments but with regtest mode (only via flag)
   docker run --rm -d -p 9085:9085 -p 9084:9084 -p 9091:9091 ghcr.io/topl/bifrost-node:2.0.0-beta3-24-7fd725a9 -- --regtest

   */

  // works without staker arguments
  // does not work with staker arguments (regtest not enabled, without flag and disabled in config file).
  // does not work with staker arguments (regtest enabled)
  // Staker arguments include volume mounting and config
  // "does not work" most of the time is TimeoutException.
  // try old version but with staker arguments? => does not work
  test("control") {
    import cats.effect.std.Queue
    assertIO(for {
      randomCall <- bifrostQuery1.blockByDepth(0)
      _ <- IO.println((randomCall)) // to verify that its not ALL rpc functions that are broken.
      _ <- IO.println("starting stream")
      monitor <- BifrostMonitor(bifrostQuery1) // monitoring node 1
      blockStream = monitor.monitorBlocks().through(s => s.map(r => {
        println(r)
        r
      }))
      _ <- IO.println("make block")
      _ <- bifrostQuery1.makeBlock(1)
      _ <- IO.println("make block")
      _ <- bifrostQuery1.makeBlock(1)
      _ <- IO.println("before interrupt")
//      blocks <- (IO.println("delayed block by depth").andWait(22.seconds) *> bifrostQuery1.blockByDepth(0)) &> blockStream.interruptAfter(20.seconds).compile.toList
      blocks <- blockStream.interruptAfter(20.seconds).compile.toList
      _ <- IO.println("D")
    } yield {
      println(blocks)
      true
    },
      true
    )
  }

  test("Monitor only new blocks (empty)") {
    assertIO(for {
      monitor <- BifrostMonitor(bifrostQuery1)
      blockStream = monitor.monitorBlocks()
      // At approx. 1 block/10 seconds, we expect there to be at least 2 blocks after 30 seconds
      blocks <- blockStream.interruptAfter(30.seconds).compile.toList
    } yield blocks.count(_.isInstanceOf[AppliedBifrostBlock]) >= 2,
      true
    )
  }

  test("Monitor only new blocks (trivial transaction)") {
    val heightLockTemplate = PredicateTemplate(Seq(HeightTemplate[IO]("header",  1, Long.MaxValue)), 1)
    val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource1)
    val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)
    assertIO(for {
      heightLock <- heightLockTemplate.build(Nil).map(_.toOption.get)
      heightAddress <- txBuilder.lockAddress(heightLock)
      txos <- genusQueryApi.queryUtxo(heightAddress)
      tx <- txBuilder.buildTransferAmountTransaction(
        LvlType,
        txos,
        heightLock.getPredicate,
        100L,
        heightAddress, // Trivial, resend to genesis address
        heightAddress,
        1L
      ).map(_.toOption.get)
      proof <- Prover.heightProver[IO].prove((), tx.signable)
      provedTx = tx.withInputs(tx.inputs.map(in => in.withAttestation(Attestation().withPredicate(in.attestation.getPredicate.withResponses(Seq(proof))))))
      // Start monitoring
      monitor <- BifrostMonitor(bifrostQuery1)
      blockStream = monitor.monitorBlocks()
      // Then broadcast transaction
      txId <- bifrostQuery1.broadcastTransaction(provedTx)
      // At approx. 1 block/10 seconds, we expect the broadcasted tx to be captured after 20 seconds
      blockUpdates <- blockStream.through(_.map(b => (b.block.transactions.map(_.computeId), b.height))).interruptAfter(20.seconds).compile.toList
      // Ensure the reported height is correct
      queriedHeights <- blockUpdates.map(b => bifrostQuery1.blockByHeight(b._2).map(r => (r.get._4.map(_.computeId), r.get._2.height))).sequence
    } yield {
      val foundUpdate = blockUpdates.find(_._1.contains(txId))
      foundUpdate.isDefined && queriedHeights.exists(_._2 == foundUpdate.get._2) &&
        (foundUpdate.get._1 equals queriedHeights.find(_._2 == foundUpdate.get._2).get._1)
    },
      true
    )
  }

  test("Monitor live and retroactive blocks") {
    assertIO(for {
      startingBlockId <- bifrostQuery1.blockByDepth(0).map(_.get._1)
      startingHeight <- bifrostQuery1.blockById(startingBlockId).map(_.get._2.height)
      // At approx. 1 block/10 seconds, we expect there to be at least 2 blocks after 30 seconds
      _ <- IO.unit.andWait(30.seconds)
      tipBlockId <- bifrostQuery1.blockByDepth(1).map(_.get._1)
      monitor <- BifrostMonitor(bifrostQuery1, Some(startingBlockId))
      blockStream = monitor.monitorBlocks()
      // At approx. 1 block/10 seconds, we expect there to be at least 2 blocks after 30 seconds
      blocks <- blockStream.interruptAfter(30.seconds).compile.toList
    } yield {
      val blockIds = blocks.map(_.id)
      val startingIdx = blockIds.indexOf(startingBlockId)
      val tipBlockIdx = blockIds.indexOf(tipBlockId)
      val expectedHeights = Seq.range(startingHeight, startingHeight + blocks.length).toList
      blocks.head.id == startingBlockId &&
        blocks.slice(startingIdx, tipBlockIdx+1).length >= 2 &&
        blocks.slice(tipBlockIdx+1, blocks.length).length >= 2 &&
        (blocks.map(_.height) equals expectedHeights) && // ensure heights are correct
        blockIds.distinct.length == blockIds.length // ensure no duplicate reporting
    },
      true
    )
  }

}
