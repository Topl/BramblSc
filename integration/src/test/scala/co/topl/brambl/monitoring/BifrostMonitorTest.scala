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
    RpcChannelResource.channelResource[IO]("localhost", 9184, secureConnection = false)
  val channelResource2: Resource[IO, ManagedChannel] =
    RpcChannelResource.channelResource[IO]("localhost", 9086, secureConnection = false)
  val bifrostQuery1: BifrostQueryAlgebra[IO] = BifrostQueryAlgebra.make[IO](channelResource1)
  val bifrostQuery2: BifrostQueryAlgebra[IO] = BifrostQueryAlgebra.make[IO](channelResource2)


  test("Monitor blocks with a reorg") {
    assertIO(
      BifrostMonitor("localhost", 9184, secureConnection = false, bifrostQuery1).use(blockStream => {
        blockStream.interruptAfter(80.seconds).compile.toList <& (for {
          // ensure the 2 nodes are in sync prior to starting
          _ <- IO.println("connecting the nodes")
          _ <- connectBifrostNodes("bifrost02", "bifrost01").start.andWait(20.seconds)
          _ <- IO.println("after connect")
          node1Tip <- bifrostQuery1.blockByDepth(0)
          _ <- IO.println(node1Tip)
          node2Tip <- bifrostQuery2.blockByDepth(0)
          _ <- IO.println(node2Tip)
          // The 2 nodes start disconnected
          _ <- IO.println("disconnecting the nodes")
          _ <- disconnectBifrostNodes("bifrost02").start.andWait(20.seconds)
          _ <- IO.println("starting monitor")

          _ <- IO.println("making blocks: node 1")
          _ <- bifrostQuery1.makeBlock(1)
          _ <- IO.println("making blocks: node 2")
          _ <- bifrostQuery2.makeBlock(2)

          _ <- IO.println("waiting after making blocks").andWait(15.seconds)

          // connect blocks. the monitor should unapply the 1 block, and then apply the 2 new blocks
          _ <- IO.println("connecting the nodes")
          _ <- connectBifrostNodes("bifrost02", "bifrost01").start.andWait(20.seconds)
        } yield ()) map { blocks =>
          println(s"blocks:  $blocks") // applied, unapplied, applied, applied
          blocks.length == 4 &&
            blocks.head.isInstanceOf[AppliedBifrostBlock] &&
            blocks(1).isInstanceOf[UnappliedBifrostBlock] &&
            blocks(2).isInstanceOf[AppliedBifrostBlock] &&
            blocks(3).isInstanceOf[AppliedBifrostBlock]
        }
      }), true )
  }

  test("Monitor only new blocks (empty)") {
    assertIO(
      BifrostMonitor("localhost", 9184, secureConnection = false, bifrostQuery1).use(blockStream => {
        (blockStream.interruptAfter(15.seconds).compile.toList <& bifrostQuery1.makeBlock(2)) map { blocks =>
          println(blocks)
          blocks.count(_.isInstanceOf[AppliedBifrostBlock]) == 2
        }}) ,
      true
    )
  }

  test("Monitor only new blocks (trivial transaction)") {
    val heightLockTemplate = PredicateTemplate(Seq(HeightTemplate[IO]("header",  1, Long.MaxValue)), 1)
    val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource1)
    val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)
    assertIO(
      BifrostMonitor("localhost", 9184, secureConnection = false, bifrostQuery1).use(blockStream => {
        (for {
          blockUpdates <- blockStream.through(_.map(b => (b.block.transactions.map(_.computeId), b.height))).interruptAfter(20.seconds).compile.toList
          // Ensure the reported height is correct
          queriedHeights <- blockUpdates.map(b => bifrostQuery1.blockByHeight(b._2).map(r => (r.get._4.map(_.computeId), r.get._2.height))).sequence
        } yield (blockUpdates, queriedHeights)) both (for {
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
          // Then broadcast transaction
          txId <- bifrostQuery1.broadcastTransaction(provedTx).andWait(5.seconds)
          _ <- bifrostQuery1.makeBlock(1).andWait(5.seconds)
        } yield txId) map { res =>
          val ((blockUpdates, queriedHeights), txId) = res
          val foundUpdate = blockUpdates.find(_._1.contains(txId))
          foundUpdate.isDefined && queriedHeights.exists(_._2 == foundUpdate.get._2) &&
            (foundUpdate.get._1 equals queriedHeights.find(_._2 == foundUpdate.get._2).get._1)
        }
      }),
      true
    )
  }

  test("Monitor live and retroactive blocks") {
    val startingBlockId = bifrostQuery1.blockByDepth(0).map(_.get._1).unsafeRunSync()
    assertIO(
      BifrostMonitor("localhost", 9184, secureConnection = false, bifrostQuery1, Some(startingBlockId)).use(blockStream => {
        blockStream.interruptAfter(20.seconds).compile.toList both (
        for {
          retroactiveHeight <- bifrostQuery1.blockById(startingBlockId).map(_.get._2.height)
          _ <- bifrostQuery1.makeBlock(2)
        } yield retroactiveHeight) map { res =>
          val (blocks, startingHeight) = res
          println(startingHeight)
          println(s"blocks: $blocks")
          val expectedHeights = Seq.range(startingHeight, startingHeight + blocks.length).toList
          val tests = Seq(
            blocks.headOption.map(_.id).contains(startingBlockId),// retroactive block is reported
            blocks.tail.length == 2,// ensure live blocks are reported
            (blocks.map(_.height) equals expectedHeights),// ensure heights are correct
            blocks.map(_.id).distinct.length == blocks.length// ensure no duplicate reporting
          )
          println(tests)
          tests.forall(_ == true)
        }
      }),
      true
    )
  }

}
