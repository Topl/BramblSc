package co.topl.brambl.monitoring

import cats.effect.IO
import cats.effect.kernel.Resource
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

  val channelResource: Resource[IO, ManagedChannel] =
    RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)
  val bifrostQuery: BifrostQueryAlgebra[IO] = BifrostQueryAlgebra.make[IO](channelResource)

  test("Monitor only new blocks (empty)") {
    assertIO(for {
      monitor <- BifrostMonitor(bifrostQuery)
      blockStream = monitor.monitorBlocks()
      // At approx. 1 block/10 seconds, we expect there to be at least 2 blocks after 30 seconds
      blocks <- blockStream.interruptAfter(30.seconds).compile.toList
    } yield blocks.count(_.isInstanceOf[AppliedBifrostBlock]) >= 2,
      true
    )
  }

  test("Monitor only new blocks (trivial transaction)") {
    val heightLockTemplate = PredicateTemplate(Seq(HeightTemplate[IO]("header",  1, Long.MaxValue)), 1)
    val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource)
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
      monitor <- BifrostMonitor(bifrostQuery)
      blockStream = monitor.monitorBlocks()
      // Then broadcast transaction
      txId <- bifrostQuery.broadcastTransaction(provedTx)
      // At approx. 1 block/10 seconds, we expect the broadcasted tx to be captured after 20 seconds
      txIds <- blockStream.through(_.flatMap(_.transactions.map(_.computeId))).interruptAfter(20.seconds).compile.toList
    } yield txIds.contains(txId),
      true
    )
  }
}
