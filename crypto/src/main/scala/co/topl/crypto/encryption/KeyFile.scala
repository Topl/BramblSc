package co.topl.crypto.encryption

import cats.Monad
import cats.implicits.{catsSyntaxEitherId, toFlatMapOps, toFunctorOps}
import co.topl.crypto.encryption.cipher.Cipher
import co.topl.crypto.encryption.kdf.Kdf
import co.topl.crypto.encryption.kdf.Codecs._
import co.topl.crypto.encryption.cipher.Codecs._
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.syntax._
import org.bouncycastle.util.Strings

/**
 * A KeyFile is a JSON object that contains the parameters of a KDF and a Cipher, as well as the cipher text and MAC.
 * @param kdfParams KDF parameters
 * @param cipherParams Cipher parameters
 * @param cipherText cipher text
 * @param mac MAC
 */
case class KeyFile[F[_]](kdf: Kdf[F], cipher: Cipher[F], cipherText: Array[Byte], mac: Array[Byte])

object KeyFile {

  /**
   * Create a KeyFile instance from a JSON object.
   * @param json the JSON object
   * @return a KeyFile instance
   */
  def fromJson[F[_]: Monad](json: Json): Either[DecodingFailure, KeyFile[F]] =
    Codecs.keyFileFromJson[F].decodeJson(json)

  /**
   * Decode a the cipher text of a KeyFile
   * @param keyfile the KeyFile
   * @return the decrypted data if mac is valid, otherwise [[InvalidMac]]
   */
  def decodeCipher[F[_]: Monad](
    keyfile:  KeyFile[F],
    password: Array[Byte]
  ): F[Either[InvalidMac.type, Array[Byte]]] =
    for {
      dKeyRaw     <- keyfile.kdf.deriveKey(password)
      isDKeyValid <- Mac.make(dKeyRaw, keyfile.cipherText).validateMac[F](keyfile.mac)
      decoded     <- keyfile.cipher.decrypt(keyfile.cipherText, dKeyRaw)
    } yield if (isDKeyValid) decoded.asRight else InvalidMac.asLeft

  object Codecs {

    implicit def keyFileToJson[F[_]: Monad]: Encoder[KeyFile[F]] = new Encoder[KeyFile[F]] {

      final override def apply(a: KeyFile[F]): Json = Json.obj(
        "kdf"        -> a.kdf.asJson,
        "cipher"     -> a.cipher.asJson,
        "cipherText" -> Json.fromString(Strings.fromByteArray(a.cipherText)),
        "mac"        -> Json.fromString(Strings.fromByteArray(a.mac))
      )
    }

    implicit def keyFileFromJson[F[_]: Monad]: Decoder[KeyFile[F]] = new Decoder[KeyFile[F]] {

      override def apply(c: HCursor): Result[KeyFile[F]] =
        for {
          kdf        <- c.downField("kdf").as[Kdf[F]]
          cipher     <- c.downField("cipher").as[Cipher[F]]
          cipherText <- c.downField("cipherText").as[String]
          mac        <- c.downField("mac").as[String]
        } yield KeyFile[F](kdf, cipher, Strings.toByteArray(cipherText), Strings.toByteArray(mac))
    }
  }

  sealed trait InvalidKeyFileFailure
  case object InvalidMac extends InvalidKeyFileFailure
}
