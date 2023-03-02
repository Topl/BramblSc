package co.topl.brambl

import cats.Id
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.models._
import co.topl.brambl.models.box.{Lock, Value}
import co.topl.brambl.models.transaction._
import co.topl.brambl.routines.digests.Blake2b256Digest
import co.topl.brambl.routines.signatures.Ed25519Signature
import co.topl.quivr.api.{Proposer, Prover}
import com.google.protobuf.ByteString
import quivr.models.{Int128, Preimage, SignableBytes, SmallData}

trait MockHelpers {

  val MockSecret: Array[Byte] = "A mock secret".getBytes
  val MockPreimage: Preimage = Preimage(ByteString.copyFrom(MockSecret), ByteString.copyFromUtf8("salt"))

  val txDatum: Datum.IoTransaction = Datum.IoTransaction(
    Event
      .IoTransaction(
        Schedule(3, 50, 100),
        List(),
        List(),
        SmallData(ByteString.copyFrom("metadata".getBytes))
      )
  )

  // Arbitrary Transaction that any new transaction can reference
  val dummyTx: IoTransaction = IoTransaction(datum = txDatum)

  val dummyTxIdentifier: KnownIdentifier.TransactionOutput32 =
    KnownIdentifier.TransactionOutput32(0, 0, 0, Identifier.IoTransaction32(dummyTx.sized32Evidence))

  val outDatum: Datum.UnspentOutput =
    Datum.UnspentOutput(Event.UnspentTransactionOutput(SmallData(ByteString.copyFrom("metadata".getBytes))))

  val inDatum: Datum.SpentOutput =
    Datum.SpentOutput(Event.SpentTransactionOutput(SmallData(ByteString.copyFrom("metadata".getBytes))))

  val value: Value =
    Value().withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(1).toByteArray))))

  val trivialOutLock: Lock =
    Lock().withPredicate(Lock.Predicate(List(Proposer.tickProposer[Id].propose(5, 15)), 1))

  val address: Address =
    Address(0, 0, Identifier().withLock32(Identifier.Lock32(trivialOutLock.sized32Evidence)))
  val knownId: KnownIdentifier = KnownIdentifier().withTransactionOutput32(dummyTxIdentifier)

  val inLockFull: Lock.Predicate = Lock.Predicate(
    List(
      Proposer.LockedProposer[Id].propose(None),
      Proposer
        .digestProposer[Id]
        .propose(
          (
            // Hardcoding Blake2b256Digest
            Blake2b256Digest.routine,
            Blake2b256Digest.hash(MockPreimage)
          )
        ),
      Proposer
        .signatureProposer[Id]
        .propose(
          (
            // Hardcoding Ed25519Signature
            Ed25519Signature.routine,
            Ed25519Signature.createKeyPair(MockSecret).vk // Get will be removed once validation rules are in place
          )
        ),
      Proposer.heightProposer[Id].propose(("header", 0, 100)),
      Proposer.tickProposer[Id].propose((0, 100))
    ),
    3
  )
  val fakeMsgBind: SignableBytes = "transaction binding".getBytes.immutable.signable

  val nonEmptyAttestation: Attestation = Attestation().withPredicate(
    Attestation.Predicate(
      inLockFull,
      List(
        Prover.lockedProver[Id].prove((), fakeMsgBind),
        Prover.heightProver[Id].prove((), fakeMsgBind),
        Prover.tickProver[Id].prove((), fakeMsgBind)
      )
    )
  )

  val output: UnspentTransactionOutput = UnspentTransactionOutput(address, value, outDatum)

  val attFull: Attestation = Attestation().withPredicate(Attestation.Predicate(inLockFull, List()))

  val inputFull: SpentTransactionOutput =
    SpentTransactionOutput(knownId, attFull, value, inDatum, List())

  val txFull: IoTransaction = IoTransaction(List(inputFull), List(output), txDatum)
}
