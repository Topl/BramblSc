package co.topl.crypto.encryption

import io.circe.Json

/**
 * Ciphers are used to encrypt and decrypt data.
 * @see [[https://en.wikipedia.org/wiki/Cipher]]
 */
package object cipher {

  /**
   * Cipher parameters.
   */
  trait Params[F[_]] {
    def asJson: Json
  }

  /**
   * A Cipher.
   */
  trait Cipher[F[_], P <: Params[F]] {

    /**
     * Encrypt data.
     * @param plainText data to encrypt
     * @param key encryption key
     * @param params cipher parameters
     * @return encrypted data
     */
    def encrypt(plainText: Array[Byte], key: Array[Byte], params: P): F[Array[Byte]]

    /**
     * Decrypt data.
     * @param cipherText data to decrypt
     * @param key decryption key
     * @param params cipher parameters
     * @return decrypted data
     */
    def decrypt(cipherText: Array[Byte], key: Array[Byte], params: P): F[Array[Byte]]
  }
}
