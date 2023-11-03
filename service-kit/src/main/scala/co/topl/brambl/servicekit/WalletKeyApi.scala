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

  /**
    * Creates an instance of the WalletKeyApiAlgebra that stores the keyfile to disk.
    *
    * @return an instance of the WalletKeyApiAlgebra that stores the keyfile to disk.
    */
  def make[F[_]: Sync](): WalletKeyApiAlgebra[F] =
    new WalletKeyApiAlgebra[F] {

      case class DecodeVaultStoreException(msg: String, t: Throwable) extends WalletKeyException(msg, t)

      case class VaultStoreDoesNotExistException(name: String)
          extends WalletKeyException(s"VaultStore at $name does not exist")

      case class MnemonicDoesNotExistException(name: String)
          extends WalletKeyException(s"Mnemonic file at $name does not exist")

      override def updateMainKeyVaultStore(
        mainKeyVaultStore: VaultStore[F],
        name:              String
      ): F[Either[WalletKeyException, Unit]] =
        if (Paths.get(name).toFile.exists())
          saveMainKeyVaultStore(mainKeyVaultStore, name) // overwrite keyfile
        else
          Either.left[WalletKeyException, Unit](VaultStoreDoesNotExistException(name)).pure[F]
      override def deleteMainKeyVaultStore(
        name: String
      ): F[Either[WalletKeyException, Unit]] =
        if (Files.deleteIfExists(Paths.get(name)))
          ().asRight[WalletKeyException].pure[F]
        else
          Either.left[WalletKeyException, Unit](VaultStoreDoesNotExistException(name)).pure[F]

      override def saveMainKeyVaultStore(
        mainKeyVaultStore: VaultStore[F],
        name:              String
      ): F[Either[WalletKeyException, Unit]] = Resource
        .make(Sync[F].delay(new PrintWriter(name)))(file => Sync[F].delay(file.flush()) >> Sync[F].delay(file.close()))
        .use { file =>
          for {
            res <- Sync[F].blocking(file.write(mainKeyVaultStore.asJson.noSpaces))
          } yield res.asRight[WalletKeyException]
        }

      override def getMainKeyVaultStore(
        name: String
      ): F[Either[WalletKeyException, VaultStore[F]]] =
        if (Paths.get(name).toFile.exists())
          Resource
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
        else Either.left[WalletKeyException, VaultStore[F]](VaultStoreDoesNotExistException(name)).pure[F]


      override def saveMnemonic(
        mnemonic:     IndexedSeq[String],
        mnemonicName: String
      ): F[Either[WalletKeyException, Unit]] =
        if (!Paths.get(mnemonicName).toFile.exists())
          Resource
            .make(Sync[F].delay(new PrintWriter(mnemonicName)))(file =>
              Sync[F].delay(file.flush()) >> Sync[F].delay(file.close())
            )
            .use { file =>
              for {
                res <- Sync[F].blocking(file.write(mnemonic.mkString(",")))
              } yield res.asRight[WalletKeyException]
            }
        else Either.left[WalletKeyException, Unit](MnemonicDoesNotExistException(mnemonicName)).pure[F]
    }
}
