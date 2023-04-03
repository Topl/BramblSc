package co.topl.crypto.encryption

import cats.Applicative
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEitherId, toFunctorOps}
import co.topl.crypto.encryption.cipher.Cipher
import co.topl.crypto.encryption.kdf.Kdf
import io.circe.Json
import io.circe.syntax._

/**
 * A KeyFile is a JSON object that contains the parameters of a KDF and a Cipher, as well as the cipher text and MAC.
 * @param kdfParams KDF parameters
 * @param cipherParams Cipher parameters
 * @param cipherText cipher text
 * @param mac MAC
 */
case class KeyFile[F[_]: Applicative](
  kdf:        Kdf[F],
  cipher:     Cipher[F],
  cipherText: Array[Byte],
  mac:        Array[Byte]
) {

  /**
   * Encode the KeyFile to it's JSON representation.
   */
  def asJson: Json = Json.obj(
    "kdf"        -> kdf.params.asJson,
    "cipher"     -> cipher.params.asJson,
    "cipherText" -> cipherText.asJson,
    "mac"        -> mac.asJson
  )

  /**
   * Decode the cipher text of the KeyFile.
   * @param password password to use to decrypt the cipher text
   * @return decrypted data
   */
  def decodeCipher(password: Array[Byte]): F[Either[KeyFile.InvalidMac.type, Array[Byte]]] =
    KeyFile.decodeCipher[F](this, password)
}

object KeyFile {

  /**
   * Decode a the cipher text of a KeyFile
   * @param keyfile the KeyFile
   * @return the decrypted data
   */
  def decodeCipher[F[_]: Applicative](
    keyfile:  KeyFile[F],
    password: Array[Byte]
  ): F[Either[InvalidMac.type, Array[Byte]]] =
    for {
      dKey <- keyfile.kdf
        .deriveKey(password)
        .map(dKey =>
          if (Mac.validateKeyFileMac(dKey, keyfile.cipherText, keyfile.mac))
            Right(dKey)
          else Left(InvalidMac)
        )
      decoded = dKey.map(keyfile.cipher.decrypt(keyfile.cipherText, _))
    } yield decoded

  def fromJson[F[_]: Applicative](json: Json): KeyFile[F] = ???

  /**
   * Decode a KeyFile from it's JSON representation.
   *
   * @param json JSON representation of the KeyFile
   * @return the decoded KeyFile
   */
  def apply[F[_]: Applicative](json: Json): KeyFile[F] = fromJson(json)

  sealed trait InvalidKeyFileFailure
  case object InvalidMac extends InvalidKeyFileFailure
  case object InvalidCipher extends InvalidKeyFileFailure
  case object InvalidKdf extends InvalidKeyFileFailure
}
