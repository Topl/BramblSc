package co.topl.brambl

import cats.Id
import co.topl.brambl.dataApi.DataApi
import co.topl.brambl.dataApi.DataApi.DataApiException
import co.topl.brambl.models._
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.UnspentTransactionOutput
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
    MockIndices -> inLockFull
  )

  val txoAddrToIdx: Map[TransactionOutputAddress, Indices] = Map(
    dummyTxoAddress -> MockIndices
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

  var mainKeyVaultStoreInstance: Map[String, Json] = Map()
  case object MainKeyVaultStoreNotInitialized extends DataApiException("MainKeyVaultStore not initialized")

  case class MainKeyVaultInvalid(cause: Throwable = null)
      extends DataApiException("Error decoding MainKeyVaultStore", cause)

  case object MainKeyVaultSaveFailure extends DataApiException("Error saving MainKeyVaultStore")

  override def saveMainKeyVaultStore(
    mainKeyVaultStore: VaultStore[Id],
    name:              String = "default"
  ): Id[Either[DataApiException, Unit]] =
    if ("error".equals(name)) Left(MainKeyVaultSaveFailure) // Mocking a save failure
    else {
      mainKeyVaultStoreInstance += (name -> mainKeyVaultStore.asJson)
      Right(())
    }

  override def getMainKeyVaultStore(
    name: String = "default"
  ): Id[Either[DataApiException, VaultStore[Id]]] =
    if (mainKeyVaultStoreInstance.getOrElse(name, Json.Null).isNull)
      Left(MainKeyVaultStoreNotInitialized)
    else
      mainKeyVaultStoreInstance(name)
        .as[VaultStore[Id]]
        .left
        .map(MainKeyVaultInvalid(_))

  override def updateMainKeyVaultStore(
    mainKeyVaultStore: VaultStore[Id],
    name:              String = "default"
  ): Id[Either[DataApiException, Unit]] =
    if (
      mainKeyVaultStoreInstance.getOrElse(name, Json.Null).isNull
    ) // not using getMainKeyVaultStore since it's okay if the existing VaultStore is invalid
      Left(MainKeyVaultStoreNotInitialized) // if the existing VaultStore does not exist, return an error
    else saveMainKeyVaultStore(mainKeyVaultStore, name)
}
