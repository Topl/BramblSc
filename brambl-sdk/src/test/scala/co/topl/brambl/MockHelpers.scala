package co.topl.brambl

import cats.Id
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.models._
import co.topl.brambl.models.box.Attestation
import co.topl.brambl.models.box.Challenge
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction._
import co.topl.crypto.hash.Blake2b256
import co.topl.quivr.api.Proposer
import co.topl.quivr.api.Prover
import com.google.protobuf.ByteString
import quivr.models.{
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

trait MockHelpers {
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

  val MockSignatureProposition: Id[Proposition] =
    Proposer.signatureProposer[Id].propose(("ExtendedEd25519", MockChildKeyPair.vk))

  val MockSignature: Witness = Witness(
    ByteString.copyFrom((new ExtendedEd25519).sign(MockChildKeyPair.signingKey, fakeMsgBind.value.toByteArray))
  )
  val MockSignatureProof: Id[Proof] = Prover.signatureProver[Id].prove(MockSignature, fakeMsgBind)

  val MockPreimage: Preimage = Preimage(ByteString.copyFrom("secret".getBytes), ByteString.copyFromUtf8("salt"))

  // Hardcoding Blake2b256
  val MockDigest: Digest =
    Digest(ByteString.copyFrom((new Blake2b256).hash(MockPreimage.input.toByteArray ++ MockPreimage.salt.toByteArray)))
  val MockDigestProposition: Id[Proposition] = Proposer.digestProposer[Id].propose(("Blake2b256", MockDigest))
  val MockDigestProof: Id[Proof] = Prover.digestProver[Id].prove(MockPreimage, fakeMsgBind)

  val MockTickProposition: Id[Proposition] = Proposer.tickProposer[Id].propose((0, 100))
  val MockTickProof: Id[Proof] = Prover.tickProver[Id].prove((), fakeMsgBind)

  val MockHeightProposition: Id[Proposition] = Proposer.heightProposer[Id].propose(("header", 0, 100))
  val MockHeightProof: Id[Proof] = Prover.heightProver[Id].prove((), fakeMsgBind)

  val MockLockedProposition: Id[Proposition] = Proposer.LockedProposer[Id].propose(None)
  val MockLockedProof: Id[Proof] = Prover.lockedProver[Id].prove((), fakeMsgBind)

  val txDatum: Datum.IoTransaction = Datum.IoTransaction(
    Event
      .IoTransaction(
        Schedule(3, 50, 100),
        SmallData(ByteString.copyFrom("metadata".getBytes))
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

  val attFull: Attestation = Attestation().withPredicate(
    Attestation.Predicate(inPredicateLockFull, List.fill(inPredicateLockFull.challenges.length)(Proof()))
  )

  val inputFull: SpentTransactionOutput = SpentTransactionOutput(dummyTxoAddress, attFull, value)

  val txFull: IoTransaction =
    IoTransaction.defaultInstance.withInputs(List(inputFull)).withOutputs(List(output)).withDatum(txDatum)

  val mockVks: List[VerificationKey] = List(
    MockChildKeyPair.vk,
    (new ExtendedEd25519).deriveKeyPairFromSeed(Array.fill(96)(1: Byte)).vk
  )
}
