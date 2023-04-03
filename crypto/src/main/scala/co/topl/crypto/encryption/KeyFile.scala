package co.topl.crypto.encryption

import cats.{Applicative, Monad}
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEitherId, toFlatMapOps, toFunctorOps}
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
case class KeyFile[F[_]: Monad](
  kdf:        Kdf[F],
  cipher:     Cipher[F],
  cipherText: F[Array[Byte]],
  mac:        F[Array[Byte]]
) {

  /**
   * Encode the KeyFile to it's JSON representation.
   */
  def asJson: F[Json] =
    for {
      kdfParams    <- kdf.params.asJson
      cipherParams <- cipher.params.asJson
      cipherText   <- cipherText.map(_.asJson)
      mac          <- mac.map(_.asJson)
    } yield Json.obj(
      "kdf"        -> kdfParams,
      "cipher"     -> cipherParams,
      "cipherText" -> cipherText,
      "mac"        -> mac
    )

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
      cipherText  <- keyfile.cipherText
      expectedMac <- keyfile.mac
      dKeyRaw     <- keyfile.kdf.deriveKey(password)
      isDKeyValid <- Mac.make[F](dKeyRaw, cipherText).validateMac(expectedMac)
      decoded     <- keyfile.cipher.decrypt(cipherText, dKeyRaw).asRight[InvalidMac.type].pure[F]
    } yield if (isDKeyValid) decoded else InvalidMac.asLeft.pure[F]

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
