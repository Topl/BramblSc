package co.topl.brambl.servicekit

import cats.effect.kernel.{Resource, Sync}
import co.topl.brambl.dataApi.WalletKeyApiAlgebra
import co.topl.crypto.encryption.VaultStore

import java.io.PrintWriter
import scala.io.Source

object WalletKeyApi {

  def make[F[_]: Sync](): WalletKeyApiAlgebra[F] =
    new WalletKeyApiAlgebra[F] {

      override def updateMainKeyVaultStore(
        mainKeyVaultStore: VaultStore[F],
        name:              String
      ): F[Either[WalletKeyApiAlgebra.WalletKeyException, Unit]] = ???

      override def deleteMainKeyVaultStore(
        name: String
      ): F[Either[WalletKeyApiAlgebra.WalletKeyException, Unit]] = ???

      override def saveMainKeyVaultStore(
        mainKeyVaultStore: VaultStore[F],
        name:              String
      ): F[Either[WalletKeyApiAlgebra.WalletKeyException, Unit]] = ???

      override def getMainKeyVaultStore(
        name: String
      ): F[Either[WalletKeyApiAlgebra.WalletKeyException, VaultStore[F]]] = ???
    }
}
