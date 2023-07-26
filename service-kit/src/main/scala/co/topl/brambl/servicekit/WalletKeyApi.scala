package co.topl.brambl.servicekit

import cats.effect.kernel.{Resource, Sync}
import co.topl.brambl.dataApi.WalletKeyApiAlgebra
import co.topl.brambl.dataApi.WalletKeyApiAlgebra.WalletKeyException
import co.topl.crypto.encryption.VaultStore
import co.topl.crypto.encryption.VaultStore.Codecs._
import io.circe.syntax._
import io.circe.parser.decode
import cats.implicits._

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import scala.io.Source

/**
 * An implementation of the WalletKeyApiAlgebra that stores the keyfile to disk.
 */
object WalletKeyApi {

  def make[F[_]: Sync](): WalletKeyApiAlgebra[F] =
    new WalletKeyApiAlgebra[F] {

      case class DecodeVaultStoreException(msg: String, t: Throwable) extends WalletKeyException(msg, t)

      case class VaultStoreDoesNotExistException(name: String)
          extends WalletKeyException(s"VaultStore at $name does not exist")

      /**
       * Updates the main key vault store.
       * @param mainKeyVaultStore The new VaultStore to update to.
       * @param name              The filepath of the VaultStore to update.
       * @return nothing if successful. An exception if the VaultStore does not exist.
       */
      override def updateMainKeyVaultStore(
        mainKeyVaultStore: VaultStore[F],
        name:              String
      ): F[Either[WalletKeyException, Unit]] =
        if (Paths.get(name).toFile.exists())
          saveMainKeyVaultStore(mainKeyVaultStore, name) // overwrite keyfile
        else
          Either.left[WalletKeyException, Unit](VaultStoreDoesNotExistException(name)).pure[F]

      /**
       * Deletes the main key vault store.
       * @param name The filepath of the VaultStore to delete.
       * @return nothing if successful. An exception if the VaultStore does not exist.
       */
      override def deleteMainKeyVaultStore(
        name: String
      ): F[Either[WalletKeyException, Unit]] =
        if (Files.deleteIfExists(Paths.get(name)))
          ().asRight[WalletKeyException].pure[F]
        else
          Either.left[WalletKeyException, Unit](VaultStoreDoesNotExistException(name)).pure[F]

      /**
       * Persists the main key vault store to disk.
       * @param mainKeyVaultStore The VaultStore to persist
       * @param name              The filepath to persist the VaultStore to.
       *  @return nothing if successful. If persisting fails due to an underlying cause, return a WalletKeyException
       */
      override def saveMainKeyVaultStore(
        mainKeyVaultStore: VaultStore[F],
        name:              String
      ): F[Either[WalletKeyException, Unit]] = Resource
        .make(Sync[F].delay(new PrintWriter(name)))(file => Sync[F].delay(file.close()))
        .use { file =>
          for {
            res <- Sync[F].blocking(file.write(mainKeyVaultStore.asJson.noSpaces))
          } yield res.asRight[WalletKeyException]
        }

      /**
       * Retrieves the main key vault store from disk.
       * @param name The filepath of the VaultStore to retrieve.
       *  @return The VaultStore for the Topl Main Secret Key if it exists.
       *          If retrieving fails due to an underlying cause, return a WalletKeyException
       */
      override def getMainKeyVaultStore(
        name: String
      ): F[Either[WalletKeyException, VaultStore[F]]] = Resource
        .make(Sync[F].delay(Source.fromFile(name))) { file =>
          Sync[F].delay(file.close())
        }
        .use { file =>
          for {
            inputString <- Sync[F].blocking(file.getLines().mkString("\n"))
            res <- Sync[F].delay(
              decode[VaultStore[F]](inputString)
                .leftMap(e => DecodeVaultStoreException("Invalid JSON", e))
            )
          } yield res
        }
    }
}
