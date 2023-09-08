package co.topl.brambl

import cats.Id
import cats.effect.IO
import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.builders.locks.LockTemplate.PredicateTemplate
import co.topl.brambl.builders.locks.PropositionTemplate.{
  AndTemplate,
  DigestTemplate,
  HeightTemplate,
  LockedTemplate,
  NotTemplate,
  OrTemplate,
  SignatureTemplate,
  ThresholdTemplate,
  TickTemplate
}
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.models._
import co.topl.brambl.models.box.Attestation
import co.topl.brambl.models.box.Challenge
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction._
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.utils.Encoding.encodeToBase58
import co.topl.crypto.hash.Blake2b256
import co.topl.quivr.api.Proposer
import co.topl.quivr.api.Prover
import com.google.protobuf.ByteString
import quivr.models.{
  Data,
  Digest,
  Int128,
  KeyPair,
  Preimage,
  Proof,
  Proposition,
  SignableBytes,
  SmallData,
  VerificationKey,
  Witness
}
import co.topl.brambl.wallet.WalletApi.{cryptoToPbKeyPair, pbKeyPairToCryotoKeyPair}
import co.topl.crypto.generation.Bip32Indexes
import co.topl.crypto.signing.ExtendedEd25519
import co.topl.genus.services.Txo
import co.topl.genus.services.TxoState.UNSPENT
import io.circe.Json
import org.bouncycastle.util.Strings

trait MockHelpers {
  type F[A] = IO[A]

  val fakeMsgBind: SignableBytes = "transaction binding".getBytes.immutable.signable

  val MockIndices: Indices = Indices(0, 0, 0)
  // Hardcoding ExtendedEd25519
  val MockMainKeyPair: KeyPair = (new ExtendedEd25519).deriveKeyPairFromSeed(Array.fill(96)(0: Byte))

  val MockChildKeyPair: KeyPair = (new ExtendedEd25519).deriveKeyPairFromChildPath(
    pbKeyPairToCryotoKeyPair(MockMainKeyPair).signingKey,
    List(
      Bip32Indexes.HardenedIndex(MockIndices.x),
      Bip32Indexes.SoftIndex(MockIndices.y),
      Bip32Indexes.SoftIndex(MockIndices.z)
    )
  )

  val MockSigningRoutine: String = "ExtendedEd25519"

  val MockSignatureProposition: Id[Proposition] =
    Proposer.signatureProposer[Id].propose((MockSigningRoutine, MockChildKeyPair.vk))

  val MockSignature: Witness = Witness(
    ByteString.copyFrom((new ExtendedEd25519).sign(MockChildKeyPair.signingKey, fakeMsgBind.value.toByteArray))
  )
  val MockSignatureProof: Id[Proof] = Prover.signatureProver[Id].prove(MockSignature, fakeMsgBind)

  val MockPreimage: Preimage = Preimage(ByteString.copyFrom("secret".getBytes), ByteString.copyFromUtf8("salt"))

  // Hardcoding Blake2b256
  val MockDigestRoutine: String = "Blake2b256"

  val MockDigest: Digest =
    Digest(ByteString.copyFrom((new Blake2b256).hash(MockPreimage.input.toByteArray ++ MockPreimage.salt.toByteArray)))
  val MockDigestProposition: Id[Proposition] = Proposer.digestProposer[Id].propose((MockDigestRoutine, MockDigest))
  val MockDigestProof: Id[Proof] = Prover.digestProver[Id].prove(MockPreimage, fakeMsgBind)

  val MockMin: Long = 0L
  val MockMax: Long = 100L
  val MockChain: String = "header"
  val MockTickProposition: Id[Proposition] = Proposer.tickProposer[Id].propose((MockMin, MockMax))
  val MockTickProof: Id[Proof] = Prover.tickProver[Id].prove((), fakeMsgBind)

  val MockHeightProposition: Id[Proposition] = Proposer.heightProposer[Id].propose((MockChain, MockMin, MockMax))
  val MockHeightProof: Id[Proof] = Prover.heightProver[Id].prove((), fakeMsgBind)

  val MockLockedProposition: Id[Proposition] = Proposer.LockedProposer[Id].propose(None)
  val MockLockedProof: Id[Proof] = Prover.lockedProver[Id].prove((), fakeMsgBind)

  val txDatum: Datum.IoTransaction = Datum.IoTransaction(
    Event
      .IoTransaction(
        Schedule(0, Long.MaxValue, System.currentTimeMillis),
        SmallData.defaultInstance
      )
  )

  // Arbitrary Transaction that any new transaction can reference
  val dummyTx: IoTransaction = IoTransaction(datum = txDatum)

  val dummyTxIdentifier: TransactionId = TransactionId(dummyTx.sizedEvidence.digest.value)

  val dummyTxoAddress: TransactionOutputAddress =
    TransactionOutputAddress(
      0,
      0,
      0,
      dummyTxIdentifier
    )

  val value: Value =
    Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(1).toByteArray))))

  val trivialOutLock: Lock =
    Lock().withPredicate(Lock.Predicate(List(Challenge().withRevealed(Proposer.tickProposer[Id].propose(5, 15))), 1))

  val trivialLockAddress: LockAddress =
    LockAddress(0, 0, LockId(trivialOutLock.sizedEvidence.digest.value))

  val inPredicateLockFull: Lock.Predicate = Lock.Predicate(
    List(
      MockLockedProposition,
      MockDigestProposition,
      MockSignatureProposition,
      MockHeightProposition,
      MockTickProposition
    )
      .map(Challenge().withRevealed),
    3
  )

  val inLockFull: Lock = Lock().withPredicate(inPredicateLockFull)
  val inLockFullAddress: LockAddress = LockAddress(0, 0, LockId(inLockFull.sizedEvidence.digest.value))

  val inPredicateLockFullAttestation: Attestation.Predicate = Attestation.Predicate(
    inPredicateLockFull,
    List(
      MockLockedProof,
      MockDigestProof,
      MockSignatureProof,
      MockHeightProof,
      MockTickProof
    )
  )

  val nonEmptyAttestation: Attestation = Attestation().withPredicate(inPredicateLockFullAttestation)

  val output: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value)

  val fullOutput: UnspentTransactionOutput = UnspentTransactionOutput(inLockFullAddress, value)

  val attFull: Attestation = Attestation().withPredicate(
    Attestation.Predicate(inPredicateLockFull, List.fill(inPredicateLockFull.challenges.length)(Proof()))
  )

  val inputFull: SpentTransactionOutput = SpentTransactionOutput(dummyTxoAddress, attFull, value)

  val txFull: IoTransaction =
    IoTransaction.defaultInstance.withInputs(List(inputFull)).withOutputs(List(output)).withDatum(txDatum)

  val txFullAlternative: IoTransaction = txFull.copy(outputs = Seq(fullOutput))

  val inputTxo: Txo = Txo(
    fullOutput,
    UNSPENT,
    dummyTxoAddress
  )

  val mockVks: List[VerificationKey] = List(
    MockChildKeyPair.vk,
    (new ExtendedEd25519).deriveKeyPairFromSeed(Array.fill(96)(1: Byte)).vk
  )

  object ExpectedLockedProposition {
    val data: Array[Byte] = "Hello world".getBytes
    val value: LockedTemplate[Id] = LockedTemplate[Id](Data(ByteString.copyFrom(data)).some)

    val fields: List[(String, Json)] = List(
      "type" -> Json.fromString(value.propositionType.label),
      "data" -> Json.fromString(encodeToBase58(data))
    )
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedHeightProposition {
    val value: HeightTemplate[Id] = HeightTemplate[Id](MockChain, MockMin, MockMax)

    val fields: List[(String, Json)] = List(
      "type"  -> Json.fromString(value.propositionType.label),
      "chain" -> Json.fromString(MockChain),
      "min"   -> Json.fromLong(MockMin),
      "max"   -> Json.fromLong(MockMax)
    )
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedTickProposition {
    val value: TickTemplate[Id] = TickTemplate[Id](MockMin, MockMax)

    val fields: List[(String, Json)] = List(
      "type" -> Json.fromString(value.propositionType.label),
      "min"  -> Json.fromLong(MockMin),
      "max"  -> Json.fromLong(MockMax)
    )
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedDigestProposition {
    val value: DigestTemplate[Id] = DigestTemplate[Id](MockDigestRoutine, MockDigest)

    val fields: List[(String, Json)] = List(
      "type"    -> Json.fromString(value.propositionType.label),
      "routine" -> Json.fromString(MockDigestRoutine),
      "digest"  -> Json.fromString(encodeToBase58(MockDigest.value.toByteArray))
    )
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedSignatureProposition {
    def value(entityIdx: Int): SignatureTemplate[Id] = SignatureTemplate[Id](MockSigningRoutine, entityIdx)

    def fields(entityIdx: Int): List[(String, Json)] = List(
      "type"      -> Json.fromString(value(entityIdx).propositionType.label),
      "routine"   -> Json.fromString(MockSigningRoutine),
      "entityIdx" -> Json.fromInt(entityIdx)
    )
    def json(entityIdx: Int): Json = Json.fromFields(fields(entityIdx))
  }

  object ExpectedAndProposition {

    val value: AndTemplate[Id] = AndTemplate[Id](
      ExpectedSignatureProposition.value(0),
      ExpectedSignatureProposition.value(1)
    )

    val fields: List[(String, Json)] = List(
      "type"  -> Json.fromString(value.propositionType.label),
      "left"  -> ExpectedSignatureProposition.json(0),
      "right" -> ExpectedSignatureProposition.json(1)
    )

    val json: Json = Json.fromFields(fields)
  }

  object ExpectedOrProposition {

    val value: OrTemplate[Id] = OrTemplate[Id](
      ExpectedLockedProposition.value,
      ExpectedTickProposition.value
    )

    val fields: List[(String, Json)] = List(
      "type"  -> Json.fromString(value.propositionType.label),
      "left"  -> ExpectedLockedProposition.json,
      "right" -> ExpectedTickProposition.json
    )

    val json: Json = Json.fromFields(fields)
  }

  object ExpectedNotProposition {
    val value: NotTemplate[Id] = NotTemplate[Id](ExpectedHeightProposition.value)

    val fields: List[(String, Json)] = List(
      "type"          -> Json.fromString(value.propositionType.label),
      "innerTemplate" -> ExpectedHeightProposition.json
    )

    val json: Json = Json.fromFields(fields)
  }

  object ExpectedThresholdProposition {

    val value: ThresholdTemplate[Id] = ThresholdTemplate[Id](
      List(
        ExpectedAndProposition.value,
        ExpectedOrProposition.value,
        ExpectedNotProposition.value
      ),
      3
    )

    val fields: List[(String, Json)] = List(
      "type"      -> Json.fromString(value.propositionType.label),
      "threshold" -> Json.fromInt(value.threshold),
      "innerTemplates" -> Json.fromValues(
        List(
          ExpectedAndProposition.json,
          ExpectedOrProposition.json,
          ExpectedNotProposition.json
        )
      )
    )

    val json: Json = Json.fromFields(fields)
  }

  object ExpectedPredicateLock {

    val value: PredicateTemplate[Id] = PredicateTemplate[Id](
      List(
        ExpectedAndProposition.value,
        ExpectedThresholdProposition.value
      ),
      2
    )

    val fields: List[(String, Json)] = List(
      "type"      -> Json.fromString(value.lockType.label),
      "threshold" -> Json.fromInt(value.threshold),
      "innerTemplates" -> Json.fromValues(
        List(
          ExpectedAndProposition.json,
          ExpectedThresholdProposition.json
        )
      )
    )

    val json: Json = Json.fromFields(fields)
  }
}
