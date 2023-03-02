package co.topl.brambl

import cats.Id
import co.topl.brambl.common.ContainsEvidence._
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.dataApi.DataApi
import co.topl.brambl.models._
import co.topl.brambl.models.box.{Box, Lock, Value}
import co.topl.brambl.models.transaction.{IoTransaction, Schedule}
import co.topl.brambl.routines.digests.Blake2b256Digest
import co.topl.brambl.routines.signatures.{Ed25519Signature, Signing}
import co.topl.quivr.api.Proposer
import com.google.protobuf.ByteString
import quivr.models._

/**
 * *
 * Mock Implementation of the DataApi
 */
object MockDataApi extends DataApi with MockHelpers {

  // Static mappings to provide the Wallet with data
  val idxToLocks: Map[Indices, Lock.Predicate] = Map(
    Indices(0, 0, 0) -> inLockFull
  )

  val idToIdx: Map[KnownIdentifier, Indices] = Map(
    dummyTxIdentifier -> Indices(0, 0, 0)
  ).map { case (o32, v) =>
    KnownIdentifier().withTransactionOutput32(o32) -> v
  }
  override def getIndicesByKnownIdentifier(id: KnownIdentifier): Option[Indices] = idToIdx.get(id)

  override def getBoxByKnownIdentifier(id: KnownIdentifier): Option[Box] = idToIdx
    .get(id)
    .flatMap(idxToLocks.get)
    .map(Lock().withPredicate(_))
    .map(Box(_, Value().withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(1).toByteArray))))))

  override def getPreimage(idx: Indices): Option[Preimage] =
    if (
      idx.x == 0 && idx.y == 0 && idx.z == 0
    ) // Mocking that we only have access to secrets associated with a specific index
      Some(MockPreimage)
    else None

  override def getKeyPair(idx: Indices, routine: Signing): Option[KeyPair] =
    if (idx.x == 0 && idx.y == 0 && idx.z == 0) { // Mocking that we only have access to secrets associated with a specific index
      Some(routine.createKeyPair(MockSecret))
    } else None

}
