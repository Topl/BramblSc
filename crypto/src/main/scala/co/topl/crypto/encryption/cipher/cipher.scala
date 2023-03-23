package co.topl.crypto.encryption

/**
 * Ciphers are used to encrypt and decrypt data.
 * @see [[https://en.wikipedia.org/wiki/Cipher]]
 */
package object cipher {
  trait Params
  trait Cipher[P <: Params] {
    def encrypt(plainText: Array[Byte], params: P): Array[Byte]
    def decrypt(cipherText: Array[Byte], params: P): Array[Byte]
  }
}
