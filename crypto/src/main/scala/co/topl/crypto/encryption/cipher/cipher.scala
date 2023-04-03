package co.topl.crypto.encryption

import cats.Applicative
import io.circe.Json

/**
 * Ciphers are used to encrypt and decrypt data.
 * @see [[https://en.wikipedia.org/wiki/Cipher]]
 */
package object cipher {

  /**
   * Cipher parameters.
   */
  trait Params {
    // Label denoting which cipher to use
    val cipher: String
    def asJson[F[_]: Applicative]: F[Json]
  }

  /**
   * A Cipher.
   */
  trait Cipher[F[_]] {
    // Cipher parameters
    val params: Params

    /**
     * Encrypt data.
     * @param plainText data to encrypt
     * @param key encryption key
     * @return encrypted data
     */
    def encrypt(plainText: Array[Byte], key: Array[Byte]): F[Array[Byte]]

    /**
     * Decrypt data.
     * @param cipherText data to decrypt
     * @param key decryption key
     * @return decrypted data
     */
    def decrypt(cipherText: Array[Byte], key: Array[Byte]): F[Array[Byte]]
  }
}
