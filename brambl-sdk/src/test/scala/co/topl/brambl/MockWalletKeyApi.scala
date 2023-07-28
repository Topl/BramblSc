package co.topl.brambl

import cats.Id
import co.topl.brambl.dataApi.WalletKeyApiAlgebra
import co.topl.brambl.dataApi.WalletKeyApiAlgebra.WalletKeyException
import co.topl.crypto.encryption.VaultStore
import co.topl.crypto.encryption.VaultStore.Codecs._
import io.circe.Json
import io.circe.syntax.EncoderOps

/**
 * Mock Implementation of the DataApi
 */
object MockWalletKeyApi extends WalletKeyApiAlgebra[Id] with MockHelpers {

  var mainKeyVaultStoreInstance: Map[String, Json] = Map()
  var mnemonicInstance: Map[String, IndexedSeq[String]] = Map()

  override def saveMainKeyVaultStore(
    mainKeyVaultStore: VaultStore[Id],
    name:              String = "default"
  ): Id[Either[WalletKeyException, Unit]] =
    if ("error".equals(name)) Left(MainKeyVaultSaveFailure) // Mocking a save failure
    else {
      mainKeyVaultStoreInstance += (name -> mainKeyVaultStore.asJson)
      Right(())
    }

  override def getMainKeyVaultStore(
    name: String = "default"
  ): Id[Either[WalletKeyException, VaultStore[Id]]] =
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
  ): Id[Either[WalletKeyException, Unit]] =
    if (
      mainKeyVaultStoreInstance.getOrElse(name, Json.Null).isNull
    ) // not using getMainKeyVaultStore since it's okay if the existing VaultStore is invalid
      Left(MainKeyVaultStoreNotInitialized) // if the existing VaultStore does not exist, return an error
    else saveMainKeyVaultStore(mainKeyVaultStore, name)

  override def deleteMainKeyVaultStore(name: String = "default"): Id[Either[WalletKeyException, Unit]] =
    if (
      mainKeyVaultStoreInstance.getOrElse(name, Json.Null).isNull
    ) // not using getMainKeyVaultStore since it's okay if the existing VaultStore is invalid
      // if the existing VaultStore does not exist, return an error
      Left(MainKeyVaultDeleteFailure)
    else {
      mainKeyVaultStoreInstance -= name
      Right(())
    }
  case object MainKeyVaultStoreNotInitialized extends WalletKeyException("MainKeyVaultStore not initialized")

  case class MainKeyVaultInvalid(cause: Throwable = null)
      extends WalletKeyException("Error decoding MainKeyVaultStore", cause)
  case object MainKeyVaultSaveFailure extends WalletKeyException("Error saving MainKeyVaultStore")
  case object MainKeyVaultDeleteFailure extends WalletKeyException("Error deleting MainKeyVaultStore")

  override def saveMnemonic(
    mnemonic:     IndexedSeq[String],
    mnemonicName: String
  ): Id[Either[WalletKeyException, Unit]] = {
    mnemonicInstance += (mnemonicName -> mnemonic)
    Right(())
  }
}
