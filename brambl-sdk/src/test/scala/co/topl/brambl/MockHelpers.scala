package co.topl.brambl

import cats.Id
import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.dataApi.MockDataApi
import co.topl.brambl.models._
import co.topl.brambl.models.box.{Lock, Value}
import co.topl.brambl.models.transaction._
import co.topl.quivr.api.{Proposer, Prover}
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof, SignableBytes, SmallData}

trait MockHelpers {

  val outDatum: Datum.UnspentOutput =
    Datum.UnspentOutput(Event.UnspentTransactionOutput(SmallData(ByteString.copyFrom("metadata".getBytes)).some).some)

  val inDatum: Datum.SpentOutput =
    Datum.SpentOutput(Event.SpentTransactionOutput(SmallData(ByteString.copyFrom("metadata".getBytes)).some).some)

  val value: Value =
    Value().withToken(Value.Token(Int128(ByteString.copyFrom(BigInt(1).toByteArray))))

  val trivialOutLock: Lock =
    Lock().withPredicate(Lock.Predicate(List(Proposer.tickProposer[Id].propose(5, 15)), 1))

  val address: Address =
    Address(0, 0, Identifier().withLock32(Identifier.Lock32(trivialOutLock.sized32Evidence.some)).some)
  val knownId: KnownIdentifier = KnownIdentifier().withTransactionOutput32(MockDataApi.dummyTxIdentifier2a)

  val inLock: Lock.Predicate = Lock.Predicate(
    List(
      Proposer.LockedProposer[Id].propose(None),
      Proposer.heightProposer[Id].propose(("header", 0, 100)),
      Proposer.tickProposer[Id].propose((0, 100))
    ),
    1
  )
  val fakeMsgBind: SignableBytes = "transaction binding".getBytes.immutable.signable

  val txDatum: Datum.IoTransaction = Datum.IoTransaction(
    Event
      .IoTransaction(
        Schedule(3, 50, 100),
        List(),
        List(),
        SmallData(ByteString.copyFrom("metadata".getBytes)).some
      )
  )
  val output: UnspentTransactionOutput = UnspentTransactionOutput(address, value, outDatum.some)

  val inResponsesFull: Seq[Proof] = List(
    Prover.lockedProver[Id].prove((), fakeMsgBind),
    Prover.heightProver[Id].prove((), fakeMsgBind),
    Prover.tickProver[Id].prove((), fakeMsgBind)
  )
  val inResponsesEmpty: Seq[Proof] = List()

  val attFull: Attestation = Attestation().withPredicate(Attestation.Predicate(inLock, inResponsesFull))

  val inputFull: SpentTransactionOutput =
    SpentTransactionOutput(knownId, attFull, value, inDatum.some, List())

  val attEmpty: Attestation = Attestation().withPredicate(Attestation.Predicate(inLock, inResponsesEmpty))

  val inputEmpty: SpentTransactionOutput =
    SpentTransactionOutput(knownId, attEmpty, value, inDatum.some, List())

  val txFull: IoTransaction = IoTransaction(List(inputFull), List(output), txDatum)
  val txEmpty: IoTransaction = IoTransaction(List(inputEmpty), List(output), txDatum)
}
