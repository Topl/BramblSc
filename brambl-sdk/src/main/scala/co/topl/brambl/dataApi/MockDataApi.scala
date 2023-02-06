package co.topl.brambl.dataApi

import cats.implicits._
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.KnownIdentifier
import co.topl.brambl.models.box.Box
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.Schedule
import co.topl.brambl.models.Indices
import co.topl.brambl.common.ContainsEvidence
import com.google.protobuf.ByteString
import quivr.models.Preimage
import quivr.models.VerificationKey
import quivr.models.KeyPair
import co.topl.brambl.common.ContainsImmutable.instances._
import ContainsEvidence._
import cats.Id
import co.topl.brambl.routines.digests.Blake2b256Digest
import co.topl.brambl.routines.signatures.{Ed25519Signature, Signing}
import co.topl.quivr.api.Proposer
import quivr.models.Int128

/**
 * *
 * Mock Implementation of the DataApi
 */
object MockDataApi extends DataApi {

  // Arbitrary Transaction that any new transaction can reference
  private val dummyTx2a = IoTransaction(
    List(),
    List(),
    Datum.IoTransaction(Event.IoTransaction(Schedule(1, 5, 100).some, List(), List(), none).some).some
  )

  private val dummyTx2b = IoTransaction(
    List(),
    List(),
    Datum.IoTransaction(Event.IoTransaction(Schedule(2, 50, 100).some, List(), List(), none).some).some
  )

  private val dummyTx3 = IoTransaction(
    List(),
    List(),
    Datum.IoTransaction(Event.IoTransaction(Schedule(3, 50, 100).some, List(), List(), none).some).some
  )

  private val dummyTx4 = IoTransaction(
    List(),
    List(),
    Datum.IoTransaction(Event.IoTransaction(Schedule(4, 50, 100).some, List(), List(), none).some).some
  )

  private val dummyTx5 = IoTransaction(
    List(),
    List(),
    Datum.IoTransaction(Event.IoTransaction(Schedule(5, 50, 100).some, List(), List(), none).some).some
  )

  private def transactionId(transaction: IoTransaction) =
    Identifier.IoTransaction32(transaction.sized32Evidence.some)

  val dummyTxIdentifier2a: KnownIdentifier.TransactionOutput32 =
    KnownIdentifier.TransactionOutput32(0, 0, 0, transactionId(dummyTx2a).some)

  val dummyTxIdentifier2b: KnownIdentifier.TransactionOutput32 =
    KnownIdentifier.TransactionOutput32(0, 0, 0, transactionId(dummyTx2b).some)

  val dummyTxIdentifier3: KnownIdentifier.TransactionOutput32 =
    KnownIdentifier.TransactionOutput32(0, 0, 0, transactionId(dummyTx3).some)

  val dummyTxIdentifier4: KnownIdentifier.TransactionOutput32 =
    KnownIdentifier.TransactionOutput32(0, 0, 0, transactionId(dummyTx5).some)

  val dummyTxIdentifier5: KnownIdentifier.TransactionOutput32 =
    KnownIdentifier.TransactionOutput32(0, 0, 0, transactionId(dummyTx4).some)

  // Static mappings to provide the Wallet with data

  val idxToLocks: Map[Indices, Lock.Predicate] = Map(
    Indices(1, 2, 0) -> buildPredicate(2, Indices(1, 2, 0)),
    Indices(0, 2, 1) -> buildPredicate(2, Indices(0, 2, 1)),
    Indices(0, 3, 0) -> buildPredicate(3, Indices(0, 3, 0)),
    Indices(1, 4, 0) -> buildPredicate(4, Indices(1, 4, 0)),
    Indices(1, 5, 0) -> buildPredicate(5, Indices(1, 5, 0))
  )

  val idToIdx: Map[KnownIdentifier, Indices] = Map(
    dummyTxIdentifier2a -> Indices(1, 2, 0), // with data
    dummyTxIdentifier2b -> Indices(0, 2, 1), // without data
    dummyTxIdentifier3  -> Indices(0, 3, 0), // without data
    dummyTxIdentifier4  -> Indices(1, 4, 0), // with data
    dummyTxIdentifier5  -> Indices(1, 5, 0) // with data
  ).map { case (o32, v) =>
    KnownIdentifier().withTransactionOutput32(o32) -> v
  }

  // Hardcoding MockStorage to use Blake2b256Digest and Curve25519Signature
  // TODO: To be replaced a LockTemplateBuilder?
  private def buildPredicate(threshold: Int, idx: Indices): Lock.Predicate = Lock.Predicate(
    List(
      Proposer.LockedProposer[Id].propose(None),
      Proposer
        .digestProposer[Id]
        .propose(
          (
            Blake2b256Digest.routine,
            Blake2b256Digest.hash(
              getPreimage(idx)
                .getOrElse(Preimage(ByteString.copyFromUtf8("unsolvable preimage"), ByteString.copyFromUtf8("salt")))
            )
          )
        ),
      Proposer
        .signatureProposer[Id]
        .propose(
          (
            Ed25519Signature.routine,
            getKeyPair(idx, Ed25519Signature)
              .flatMap(_.vk)
              .getOrElse(VerificationKey(ByteString.copyFromUtf8("fake vk")))
          )
        ),
      Proposer.heightProposer[Id].propose(("header", 2, 8)),
      Proposer.tickProposer[Id].propose((2, 8))
    ),
    threshold // N of 5 predicate
  )

  private def getSecret(idx: Indices): Array[Byte] = s"${idx.x},${idx.y},${idx.z}".getBytes

  override def getIndicesByKnownIdentifier(id: KnownIdentifier): Option[Indices] = idToIdx.get(id)

  override def getBoxByKnownIdentifier(id: KnownIdentifier): Option[Box] = idToIdx
    .get(id)
    .flatMap(idxToLocks.get)
    .map(Lock().withPredicate(_))
    .map(_.some)
    .map(Box(_, Value().withToken(Value.Token(Int128(ByteString.copyFrom(BigInt(1).toByteArray)).some)).some))

  override def getPreimage(idx: Indices): Option[Preimage] =
    if (idx.x == 1) // Mocking that we only have access to secrets associated with x=1
      Some(Preimage(ByteString.copyFrom(getSecret(idx)), ByteString.copyFromUtf8("salt")))
    else None

  override def getKeyPair(idx: Indices, routine: Signing): Option[KeyPair] =
    if (idx.x == 1) { // Mocking that we only have access to secrets associated with x=1
      Some(routine.createKeyPair(getSecret(idx)))
    } else None

}
