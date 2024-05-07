package co.topl.crypto.encryption

import cats.Monad
import cats.implicits.catsSyntaxEitherId
import cats.implicits.toFlatMapOps
import cats.implicits.toFunctorOps
import co.topl.crypto.encryption.cipher.Cipher
import co.topl.crypto.encryption.cipher.Codecs._
import co.topl.crypto.encryption.kdf.Codecs._
import co.topl.crypto.encryption.kdf.Kdf
import io.circe.Decoder
import io.circe.Decoder.Result
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax._
import org.bouncycastle.util.Strings

/**
 * A VaultStore is a JSON encode-able object that contains the KDF and Cipher necessary to decrypt the cipher text.
 *
 * @param kdf the associated KDF
 * @param cipher the associated Cipher
 * @param cipherText cipher text
 * @param mac MAC to validate the data integrity
 */
case class VaultStore[F[_]](kdf: Kdf[F], cipher: Cipher[F], cipherText: Array[Byte], mac: Array[Byte]) {

  override def equals(that: Any): Boolean = that match {
    case that: VaultStore[_] =>
      kdf == that.kdf &&
      cipher == that.cipher &&
      java.util.Arrays.equals(cipherText, that.cipherText) &&
      java.util.Arrays.equals(mac, that.mac)
    case _ => false
  }

  override def hashCode(): Int =
    kdf.hashCode + cipher.hashCode + java.util.Arrays.hashCode(cipherText) + java.util.Arrays.hashCode(mac)
}

object VaultStore {

  /**
   * Create a VaultStore instance from a JSON object.
   *
   * @param json the JSON object
   * @return a VaultStore instance
   */
  def fromJson[F[_]: Monad](json: Json): Either[DecodingFailure, VaultStore[F]] =
    Codecs.vaultStoreFromJson[F].decodeJson(json)

  /**
   * Decode a the cipher text of a VaultStore
   * @param VaultStore the VaultStore
   * @return the decrypted data if mac is valid, otherwise [[InvalidMac]]
   */
  def decodeCipher[F[_]: Monad](
    VaultStore: VaultStore[F],
    password:   Array[Byte]
  ): F[Either[InvalidMac.type, Array[Byte]]] =
    for {
      dKeyRaw     <- VaultStore.kdf.deriveKey(password)
      isDKeyValid <- Mac.make(dKeyRaw, VaultStore.cipherText).validateMac[F](VaultStore.mac)
      decoded     <- VaultStore.cipher.decrypt(VaultStore.cipherText, dKeyRaw)
    } yield if (isDKeyValid) decoded.asRight else InvalidMac.asLeft

  /**
   * JSON codecs for a VaultStore
   */
  object Codecs {

    /**
     * JSON encoder for a VaultStore
     */
    implicit def vaultStoreToJson[F[_]: Monad]: Encoder[VaultStore[F]] = new Encoder[VaultStore[F]] {

      final override def apply(a: VaultStore[F]): Json = Json.obj(
        "kdf"        -> a.kdf.asJson,
        "cipher"     -> a.cipher.asJson,
        "cipherText" -> Json.fromString(Strings.fromByteArray(a.cipherText)),
        "mac"        -> Json.fromString(Strings.fromByteArray(a.mac))
      )
    }

    /**
     * JSON decoder for a VaultStore
     */
    implicit def vaultStoreFromJson[F[_]: Monad]: Decoder[VaultStore[F]] = new Decoder[VaultStore[F]] {

      override def apply(c: HCursor): Result[VaultStore[F]] =
        for {
          kdf        <- c.downField("kdf").as[Kdf[F]]
          cipher     <- c.downField("cipher").as[Cipher[F]]
          cipherText <- c.downField("cipherText").as[String]
          mac        <- c.downField("mac").as[String]
        } yield VaultStore[F](kdf, cipher, Strings.toByteArray(cipherText), Strings.toByteArray(mac))
    }
  }

  sealed trait InvalidVaultStoreFailure extends Throwable
  case object InvalidMac extends InvalidVaultStoreFailure
}
