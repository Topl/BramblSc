package co.topl.crypto.encryption

/**
 * Ciphers are used to encrypt and decrypt data.
 * @see [[https://en.wikipedia.org/wiki/Cipher]]
 */
package object cipher {
  /**
   * Cipher parameters.
   */
  trait Params
  /**
   * A Cipher.
   */
  trait Cipher[P <: Params] {
    /**
     * Encrypt data.
     * @param plainText data to encrypt
     * @param key encryption key
     * @param params cipher parameters
     * @return encrypted data
     */
    def encrypt(plainText: Array[Byte], key: Array[Byte], params: P): Array[Byte]
    /**
     * Decrypt data.
     * @param cipherText data to decrypt
     * @param key encryption key
     * @param params cipher parameters
     * @return decrypted data
     */
    def decrypt(cipherText: Array[Byte], key: Array[Byte], params: P): Array[Byte]
  }
}
