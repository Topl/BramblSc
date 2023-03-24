package co.topl.crypto.encryption.cipher

import org.bouncycastle.crypto.engines.AESEngine

/**
 * AES encryption.
 * Aes is a symmetric block cipher that can encrypt and decrypt data using the same key.
 * @see [[https://en.wikipedia.org/wiki/Advanced_Encryption_Standard]]
 */
class Aes extends Cipher[Aes.AesParams]{

  /**
   * Encrypt data.
   *
   * @note AES block size is a multiple of 16, so the data must have a length multiple of 16.
   *       Simply padding the bytes would make it impossible to determine the initial data bytes upon encryption.
   *       The length of plaintext is prepended to the plaintext, and _then_ it is padded.
   *
   * @param plainText data to encrypt
   * @param key encryption key
   * @param params cipher parameters
   * @return encrypted data
   */
  override def encrypt(plainText: Array[Byte], key: Array[Byte], params: Aes.AesParams): Array[Byte] = ???

  /**
   * Decrypt data.
   *
   * @note The outputText consists of [dataLength] ++ [data] ++ [padding]
   *
   * @param cipherText data to decrypt
   * @param key encryption key
   * @param params cipher parameters
   * @return decrypted data
   */
  override def decrypt(cipherText: Array[Byte], key: Array[Byte], params: Aes.AesParams): Array[Byte] = ???

}

object Aes {
  /**
   * AES parameters.
   */
  trait AesParams extends Params {
    val iv: Array[Byte] // initialization vector
  }

  /**
   * Generate a random initialization vector.
   * @return a random initialization vector
   */
  def generateIv: Array[Byte] = {
    val iv = new Array[Byte](16)
    new java.util.Random().nextBytes(iv)
    iv
  }
}