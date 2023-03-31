package co.topl.crypto.encryption

import io.circe.Json
import io.circe.syntax._

/**
 * A KeyFile is a JSON object that contains the parameters of a KDF and a Cipher, as well as the cipher text and MAC.
 * @param kdfParams KDF parameters
 * @param cipherParams Cipher parameters
 * @param cipherText cipher text
 * @param mac MAC
 */
case class KeyFile[F[_], KdfParams <: kdf.Params[F], CipherParams <: cipher.Params[F]](
  kdfParams:    KdfParams,
  cipherParams: CipherParams,
  cipherText:   Array[Byte],
  mac:          Array[Byte]
) {

  /**
   * Encode the KeyFile to it's JSON representation.
   */
  def asJson: Json = Json.obj(
    "kdf"        -> kdfParams.asJson,
    "cipher"     -> cipherParams.asJson,
    "cipherText" -> cipherText.asJson,
    "mac"        -> mac.asJson
  )
}

object KeyFile {

  /**
   * Decode a the cipher text of a KeyFile from it's JSON representation.
   * @param json JSON representation of the KeyFile
   * @return
   */
  def decodeCipher[F[_]](json: Json, password: Array[Byte]): F[Array[Byte]] =
    // get the cipher text from the JSON and mac from the JSON
    // get the param Jsons for kdf and cipher
    // using the labels in the param Jsons, get the Kdf and Cipher instances
    // using the Kdf instance and other kdf params, derive the key using the password
    // compare the derived key to the mac
    // using the Cipher instance and other cipher params, decrypt the cipher text the derived key
    // return the decrypted data
    ???
}
