package co.topl.brambl

import cats.Id
import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.dataApi.DataApi
import co.topl.brambl.models._
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.routines.signatures.Signing
import co.topl.crypto.encryption.VaultStore
import co.topl.crypto.encryption.VaultStore.Codecs._
import com.google.protobuf.ByteString
import io.circe.Json
import io.circe.syntax.EncoderOps
import quivr.models._

/**
 * Mock Implementation of the DataApi
 */
object MockDataApi extends DataApi[Id] with MockHelpers {

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

  var mainKeyVaultStoreInstance: Option[Json] = None
  case object MainKeyVaultStoreNotInitialized extends DataApiException("MainKeyVaultStore not initialized")

  case class MainKeyVaultInvalid(cause: Throwable = null)
      extends DataApiException("Error decoding MainKeyVaultStore", cause)

  override def saveMainKeyVaultStore(
    mainKeyVaultStore: VaultStore[Id]
  ): Id[Either[MockDataApi.DataApiException, Unit]] = {
    mainKeyVaultStoreInstance = mainKeyVaultStore.asJson.some
    Right(())
  }

  override def getMainKeyVaultStore: Id[Either[MockDataApi.DataApiException, VaultStore[Id]]] =
    if (mainKeyVaultStoreInstance.getOrElse(Json.Null).isNull)
      Left(MainKeyVaultStoreNotInitialized)
    else
      mainKeyVaultStoreInstance.get
        .as[VaultStore[Id]]
        .left
        .map(MainKeyVaultInvalid(_))
}
