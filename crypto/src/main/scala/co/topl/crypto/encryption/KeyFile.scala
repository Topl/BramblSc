package co.topl.crypto.encryption

import cats.{Applicative, Id, Monad}
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEitherId, toFlatMapOps, toFunctorOps}
import co.topl.crypto.encryption.cipher.Cipher
import co.topl.crypto.encryption.kdf.Kdf
import co.topl.crypto.encryption.kdf.Codecs._
import co.topl.crypto.encryption.cipher.Codecs._
import io.circe.Decoder.Result
import io.circe.Json
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.generic.auto._
import org.bouncycastle.util.Strings

/**
 * A KeyFile is a JSON object that contains the parameters of a KDF and a Cipher, as well as the cipher text and MAC.
 * @param kdfParams KDF parameters
 * @param cipherParams Cipher parameters
 * @param cipherText cipher text
 * @param mac MAC
 */
case class KeyFile[F[_]: Monad](
  kdf:        Kdf[F],
  cipher:     Cipher[F],
  cipherText: Array[Byte],
  mac:        Array[Byte]
) {

  /**
   * Decode the cipher text of the KeyFile.
   * @param password password to use to decrypt the cipher text
   * @return the decrypted data if mac is valid, otherwise [[KeyFile.InvalidMac]]
   */
  def decodeCipher(password: Array[Byte]): F[Either[KeyFile.InvalidMac.type, Array[Byte]]] =
    KeyFile.decodeCipher[F](this, password)
}

object KeyFile {

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
      isDKeyValid <- Mac.make[F](dKeyRaw, keyfile.cipherText).validateMac(keyfile.mac)
      decoded     <- keyfile.cipher.decrypt(keyfile.cipherText, dKeyRaw)
    } yield if (isDKeyValid) decoded.asRight else InvalidMac.asLeft

  /**
   * Decode a KeyFile from it's JSON representation.
   *
   * @param json JSON representation of the KeyFile
   * @return the decoded KeyFile
   */
//  def apply[F[_]: Applicative](json: Json): KeyFile[F] = fromJson(json)

  object Codecs {

    implicit def encodeKeyFile[F[_]: Monad]: Encoder[KeyFile[F]] = new Encoder[KeyFile[F]] {

      final override def apply(a: KeyFile[F]): Json = Json.obj(
        "kdf"        -> a.kdf.asJson,
        "cipher"     -> a.cipher.asJson,
        "cipherText" -> Json.fromString(Strings.fromByteArray(a.cipherText)),
        "mac"        -> Json.fromString(Strings.fromByteArray(a.mac))
      )
    }

    implicit def decodeKeyFile[F[_]: Monad]: Decoder[KeyFile[F]] = new Decoder[KeyFile[F]] {

      override def apply(c: HCursor): Result[KeyFile[F]] =
        for {
          kdf        <- c.downField("kdf").as[Kdf[F]]
          cipher     <- c.downField("cipher").as[Cipher[F]]
          cipherText <- c.downField("cipherText").as[String]
          mac        <- c.downField("mac").as[String]
        } yield KeyFile[F](
          kdf = kdf,
          cipher = cipher,
          cipherText = Strings.toByteArray(cipherText),
          mac = Strings.toByteArray(mac)
        )
    }
  }

  sealed trait InvalidKeyFileFailure
  case object InvalidMac extends InvalidKeyFileFailure
}
