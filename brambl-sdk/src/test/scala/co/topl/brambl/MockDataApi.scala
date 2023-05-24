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
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._

/**
 * Mock Implementation of the DataApi
 */
object MockDataApi extends DataApi[Id] with MockHelpers {

  var mainKeyVaultStoreInstance: Map[String, Json] = Map()

  val txoAddrToTxo: Map[TransactionOutputAddress, UnspentTransactionOutput] = Map(
    dummyTxoAddress -> UnspentTransactionOutput(
      inLockFullAddress,
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(1).toByteArray))))
    )
  )

  val lockAddrToLock: Map[LockAddress, Lock] = Map(inLockFullAddress -> inLockFull)

  override def getUtxoByTxoAddress(
    address: TransactionOutputAddress
  ): Either[DataApiException, UnspentTransactionOutput] =
    txoAddrToTxo.get(address).toRight(UnspentTransactionOutputNotFound)

  override def getLockByLockAddress(address: LockAddress): Either[DataApiException, Lock] =
    lockAddrToLock.get(address).toRight(LockNotFound)

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

  override def deleteMainKeyVaultStore(name: String = "default"): Id[Either[DataApiException, Unit]] =
    if (
      mainKeyVaultStoreInstance.getOrElse(name, Json.Null).isNull
    ) // not using getMainKeyVaultStore since it's okay if the existing VaultStore is invalid
      // if the existing VaultStore does not exist, return an error
      Left(MainKeyVaultDeleteFailure)
    else {
      mainKeyVaultStoreInstance -= name
      Right(())
    }

  case object IndicesNotFound extends DataApiException("Indices not found for SignatureProposition")
  case object PreimageNotFound extends DataApiException("Preimage not found for DigestProposition")
  case object LockNotFound extends DataApiException("Lock not found for LockId")
  case object UnspentTransactionOutputNotFound extends DataApiException("UTXO not found")
  case object MainKeyVaultStoreNotInitialized extends DataApiException("MainKeyVaultStore not initialized")

  case class MainKeyVaultInvalid(cause: Throwable = null)
      extends DataApiException("Error decoding MainKeyVaultStore", cause)
  case object MainKeyVaultSaveFailure extends DataApiException("Error saving MainKeyVaultStore")
  case object MainKeyVaultDeleteFailure extends DataApiException("Error deleting MainKeyVaultStore")
}
