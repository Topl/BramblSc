package co.topl.brambl

import co.topl.brambl.dataApi.DataApi
import co.topl.brambl.models._
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.routines.signatures.Signing
import com.google.protobuf.ByteString
import quivr.models._

/**
 * Mock Implementation of the DataApi
 */
object MockDataApi extends DataApi with MockHelpers {

  // Static mappings to provide the Wallet with data
  val idxToLocks: Map[Indices, Lock.Predicate] = Map(
    Indices(0, 0, 0) -> inLockFull
  )

  val txoAddrToIdx: Map[TransactionOutputAddress, Indices] = Map(
    dummyTxoAddress -> Indices(0, 0, 0)
  )

  val txoAddrToTxo: Map[TransactionOutputAddress, UnspentTransactionOutput] = Map(
    dummyTxoAddress -> UnspentTransactionOutput(
      trivialInLockFullAddress,
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(1).toByteArray))))
    )
  )

  val lockAddrToLock: Map[LockAddress, Lock] = Map(
    trivialInLockFullAddress -> Lock().withPredicate(inLockFull)
  )

  override def getIndicesByTxoAddress(address: TransactionOutputAddress): Option[Indices] = txoAddrToIdx.get(address)

  override def getUtxoByTxoAddress(address: TransactionOutputAddress): Option[UnspentTransactionOutput] =
    txoAddrToTxo.get(address)

  override def getLockByLockAddress(address: LockAddress): Option[Lock] = lockAddrToLock.get(address)

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
