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
import quivr.models.{Digest, Int128, KeyPair, Preimage, Proposition, SignableBytes, SmallData}
import co.topl.brambl.wallet.WalletApi.{cryptoToPbKeyPair, pbKeyPairToCryotoKeyPair}
import co.topl.crypto.generation.Bip32Indexes
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing.ExtendedEd25519

trait MockHelpers {
  val MockIndices: Indices = Indices(0, 0, 0)
  // Hardcoding ExtendedEd25519
  val MockMainKeyPair: KeyPair = (new ExtendedEd25519).deriveKeyPairFromEntropy(Entropy.generate(), None)

  val MockChildKeyPair: KeyPair = (new ExtendedEd25519).deriveKeyPairFromChildPath(
    pbKeyPairToCryotoKeyPair(MockMainKeyPair).signingKey,
    List(
      Bip32Indexes.HardenedIndex(MockIndices.x),
      Bip32Indexes.SoftIndex(MockIndices.y),
      Bip32Indexes.SoftIndex(MockIndices.y)
    )
  )

  val MockSignatureProposition: Id[Proposition] =
    Proposer.signatureProposer[Id].propose(("ExtendedEd25519", MockChildKeyPair.vk))

  val MockPreimage: Preimage = Preimage(ByteString.copyFrom("secret".getBytes), ByteString.copyFromUtf8("salt"))

  // Hardcoding Blake2b256
  private val MockDigest =
    Digest(ByteString.copyFrom((new Blake2b256).hash(MockPreimage.input.toByteArray ++ MockPreimage.salt.toByteArray)))
  val MockDigestProposition: Id[Proposition] = Proposer.digestProposer[Id].propose(("Blake2b256", MockDigest))

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
      Proposer.LockedProposer[Id].propose(None),
      MockDigestProposition,
      MockSignatureProposition,
      Proposer.heightProposer[Id].propose(("header", 0, 100)),
      Proposer.tickProposer[Id].propose((0, 100))
    )
      .map(Challenge().withRevealed),
    3
  )

  val inLockFull: Lock = Lock().withPredicate(inPredicateLockFull)
  val inLockFullId: LockId = LockId(inLockFull.sizedEvidence.digest.value)
  val inLockFullAddress: LockAddress = LockAddress(0, 0, inLockFullId)

  val fakeMsgBind: SignableBytes = "transaction binding".getBytes.immutable.signable

  val nonEmptyAttestation: Attestation = Attestation().withPredicate(
    Attestation.Predicate(
      inPredicateLockFull,
      List(
        Prover.lockedProver[Id].prove((), fakeMsgBind),
        Prover.heightProver[Id].prove((), fakeMsgBind),
        Prover.tickProver[Id].prove((), fakeMsgBind)
      )
    )
  )

  val output: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value)

  val attFull: Attestation = Attestation().withPredicate(Attestation.Predicate(inPredicateLockFull, List()))

  val inputFull: SpentTransactionOutput = SpentTransactionOutput(dummyTxoAddress, attFull, value)

  val txFull: IoTransaction = IoTransaction(List(inputFull), List(output), txDatum)
}
