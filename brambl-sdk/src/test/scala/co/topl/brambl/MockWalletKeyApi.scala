package co.topl.brambl

import cats.effect.IO
import co.topl.brambl.dataApi.WalletKeyApiAlgebra
import co.topl.brambl.dataApi.WalletKeyApiAlgebra.WalletKeyException
import co.topl.crypto.encryption.VaultStore
import co.topl.crypto.encryption.VaultStore.Codecs._
import io.circe.Json
import io.circe.syntax.EncoderOps

/**
 * Mock Implementation of the DataApi
 */
object MockWalletKeyApi extends WalletKeyApiAlgebra[IO] with MockHelpers {

  var mainKeyVaultStoreInstance: Map[String, Json] = Map()
  var mnemonicInstance: Map[String, IndexedSeq[String]] = Map()

  override def saveMainKeyVaultStore(
    mainKeyVaultStore: VaultStore[IO],
    name:              String = "default"
  ): IO[Either[WalletKeyException, Unit]] =
    if ("error".equals(name)) IO.pure(Left(MainKeyVaultSaveFailure)) // Mocking a save failure
    else {
      mainKeyVaultStoreInstance += (name -> mainKeyVaultStore.asJson)
      IO.pure(Right(()))
    }

  override def getMainKeyVaultStore(
    name: String = "default"
  ): IO[Either[WalletKeyException, VaultStore[IO]]] =
    if (mainKeyVaultStoreInstance.getOrElse(name, Json.Null).isNull)
      IO.pure(Left(MainKeyVaultStoreNotInitialized))
    else
      IO.pure(
        mainKeyVaultStoreInstance(name)
          .as[VaultStore[IO]]
          .left
          .map(MainKeyVaultInvalid(_))
      )

  override def updateMainKeyVaultStore(
    mainKeyVaultStore: VaultStore[IO],
    name:              String = "default"
  ): IO[Either[WalletKeyException, Unit]] =
    if (
      mainKeyVaultStoreInstance.getOrElse(name, Json.Null).isNull
    ) // not using getMainKeyVaultStore since it's okay if the existing VaultStore is invalid
      IO.pure(Left(MainKeyVaultStoreNotInitialized)) // if the existing VaultStore does not exist, return an error
    else saveMainKeyVaultStore(mainKeyVaultStore, name)

  override def deleteMainKeyVaultStore(name: String = "default"): IO[Either[WalletKeyException, Unit]] =
    if (
      mainKeyVaultStoreInstance.getOrElse(name, Json.Null).isNull
    ) // not using getMainKeyVaultStore since it's okay if the existing VaultStore is invalid
      // if the existing VaultStore does not exist, return an error
      IO.pure(Left(MainKeyVaultDeleteFailure))
    else {
      mainKeyVaultStoreInstance -= name
      IO.pure(Right(()))
    }
  case object MainKeyVaultStoreNotInitialized extends WalletKeyException("MainKeyVaultStore not initialized")

  case class MainKeyVaultInvalid(cause: Throwable = null)
      extends WalletKeyException("Error decoding MainKeyVaultStore", cause)
  case object MainKeyVaultSaveFailure extends WalletKeyException("Error saving MainKeyVaultStore")
  case object MainKeyVaultDeleteFailure extends WalletKeyException("Error deleting MainKeyVaultStore")

  override def saveMnemonic(
    mnemonic:     IndexedSeq[String],
    mnemonicName: String
  ): IO[Either[WalletKeyException, Unit]] = {
    mnemonicInstance += (mnemonicName -> mnemonic)
    IO.pure(Right(()))
  }
}
