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
import co.topl.brambl.monitoring.BifrostMonitor.{AppliedBifrostBlock, UnappliedBifrostBlock}
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
      // ensure the 2 nodes are in sync prior to starting
      _ <- IO.println("connecting the nodes")
      _ <- connectBifrostNodes("bridge", "bifrost02").start.allocated.andWait(2.seconds)
      node1Tip <- bifrostQuery1.blockByDepth(0)
      _ <- IO.println(node1Tip)
      node2Tip <- bifrostQuery2.blockByDepth(0)
      _ <- IO.println(node2Tip)
//      // The 2 nodes start disconnected
      _ <- IO.println("disconnecting the nodes")
      _ <- disconnectBifrostNodes("bridge", "bifrost02").start.allocated.andWait(2.seconds)
      _ <- IO.println("starting monitor")
      monitor <- BifrostMonitor(bifrostQuery1) // monitoring node 1
      blockStream = monitor.monitorBlocks().through(s => s.map(r => {
        println(r)
        r
      }))
      _ <- IO.println("making blocks: node 1")
      _ <- bifrostQuery1.makeBlock(1)
//      _ <- IO.println("making blocks: node 2")
      _ <- bifrostQuery2.makeBlock(2)

      // connect blocks. the monitor should unapply the 1 block, and then apply the 2 new blocks
      _ <- IO.println("connecting the nodes")
      _ <- connectBifrostNodes("bridge", "bifrost02").start.allocated.andWait(2.seconds)
      _ <- bifrostQuery2.makeBlock(1).andWait(5.seconds) // monitor should report this

      blocks <- blockStream.interruptAfter(5.seconds).compile.toList
    } yield {
      println(s"blocks:  $blocks")
      blocks.length == 5 &&
        blocks.head.isInstanceOf[AppliedBifrostBlock] &&
        blocks(1).isInstanceOf[UnappliedBifrostBlock] &&
        blocks(2).isInstanceOf[AppliedBifrostBlock] &&
        blocks(3).isInstanceOf[AppliedBifrostBlock] &&
        blocks(4).isInstanceOf[AppliedBifrostBlock]
    } // applied, unapplied, applied, applied, applied
      , true )
  }

  test("Monitor only new blocks (empty)") {
    assertIO(for {
      monitor <- BifrostMonitor(bifrostQuery1)
      blockStream = monitor.monitorBlocks()
      _ <- bifrostQuery1.makeBlock(2)
      blocks <- blockStream.interruptAfter(5.seconds).compile.toList
    } yield blocks.count(_.isInstanceOf[AppliedBifrostBlock]) == 2,
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
      _ <- bifrostQuery1.makeBlock(1)
      blockUpdates <- blockStream.through(_.map(b => (b.block.transactions.map(_.computeId), b.height))).interruptAfter(5.seconds).compile.toList
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
      _ <- bifrostQuery1.makeBlock(2)
      _ <- IO.unit.andWait(5.seconds)
      tipBlockId <- bifrostQuery1.blockByDepth(1).map(_.get._1)
      monitor <- BifrostMonitor(bifrostQuery1, Some(startingBlockId))
      blockStream = monitor.monitorBlocks()
      _ <- bifrostQuery1.makeBlock(2)
      blocks <- blockStream.interruptAfter(5.seconds).compile.toList
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
