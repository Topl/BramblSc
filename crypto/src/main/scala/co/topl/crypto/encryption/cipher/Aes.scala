package co.topl.crypto.encryption.cipher

/**
 * AES encryption.
 * Aes is a symmetric block cipher that can encrypt and decrypt data using the same key.
 * @see [[https://en.wikipedia.org/wiki/Advanced_Encryption_Standard]]
 */
class Aes extends Cipher[Aes.AesParams]{
  override def encrypt(plainText: Array[Byte], params: Aes.AesParams): Array[Byte] = ???

  override def decrypt(cipherText: Array[Byte], params: Aes.AesParams): Array[Byte] = ???
}

object Aes {
  trait AesParams extends Params {
    val iv: Array[Byte] // initialization vector
    val key : Array[Byte] // the key
  }
}