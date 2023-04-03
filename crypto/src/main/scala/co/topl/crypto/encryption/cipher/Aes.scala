package co.topl.crypto.encryption.cipher

import cats.Applicative
import cats.implicits.catsSyntaxApplicativeId
import io.circe.Json
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.modes.SICBlockCipher
import org.bouncycastle.crypto.params.{KeyParameter, ParametersWithIV}

/**
 * AES encryption.
 * Aes is a symmetric block cipher that can encrypt and decrypt data using the same key.
 * @see [[https://en.wikipedia.org/wiki/Advanced_Encryption_Standard]]
 */
object Aes {
  val BlockSize: Int = 16

  /**
   * Generate a random initialization vector.
   *
   * @return a random initialization vector
   */
  def generateIv: Array[Byte] = {
    val iv = new Array[Byte](BlockSize)
    new java.util.Random().nextBytes(iv)
    iv
  }

  /**
   * AES parameters.
   *
   * @param iv initialization vector
   */
  case class AesParams(iv: Array[Byte]) extends Params {
    override val cipher: String = "aes"

    override def asJson: Json = Json.obj(
      "iv"     -> Json.fromString(iv.map("%02x" format _).mkString),
      "cipher" -> Json.fromString(cipher)
    )
  }

  /**
   * Create an instance of the AES cipher.
   */
  def make[F[_]: Applicative](aesParams: AesParams): Cipher[F] = new Cipher[F] {
    override val params: AesParams = aesParams

    /**
     * Encrypt data.
     *
     * @note AES block size is a multiple of 16, so the data must have a length multiple of 16.
     *       Simply padding the bytes would make it impossible to determine the initial data bytes upon encryption.
     *       The amount padded to the plaintext is prepended to the plaintext. Since we know the amount padded is
     *       <16, only one byte is needed to store the amount padded.
     * @param plainText data to encrypt
     * @param key       the symmetric key for encryption and decryption
     *                  Must be 128/192/256 bits or 16/24/32 bytes.
     * @return encrypted data
     */
    override def encrypt(plainText: Array[Byte], key: Array[Byte]): F[Array[Byte]] = {
      // + 1 to account for the byte storing the amount padded. This value is guaranteed to be <16
      val amountPadded = (Aes.BlockSize - ((plainText.length + 1) % Aes.BlockSize)) % Aes.BlockSize
      val paddedBytes = amountPadded.toByte +: plainText ++: Array.fill[Byte](amountPadded)(0)
      processAes(paddedBytes, key, params.iv, encrypt = true).pure[F]
    }

    /**
     * Decrypt data.
     *
     * @note The preImage consists of [paddedAmount] ++ [data] ++ [padding]
     * @param cipherText data to decrypt
     * @param key        the symmetric key for encryption and decryption
     *                   Must be 128/192/256 bits or 16/24/32 bytes.
     * @return decrypted data
     */
    override def decrypt(cipherText: Array[Byte], key: Array[Byte]): F[Array[Byte]] = {
      val preImage = processAes(cipherText, key, params.iv, encrypt = false)
      val paddedAmount = preImage.head.toInt
      val paddedBytes = preImage.tail
      paddedBytes.slice(0, paddedBytes.length - paddedAmount).pure[F]
    }

    private def processAes(input: Array[Byte], key: Array[Byte], iv: Array[Byte], encrypt: Boolean): Array[Byte] = {
      val cipherParams = new ParametersWithIV(new KeyParameter(key), iv)
      val aesCtr = new BufferedBlockCipher(new SICBlockCipher(new AESEngine))
      aesCtr.init(encrypt, cipherParams)
      val output = Array.fill[Byte](input.length)(1: Byte)
      aesCtr.processBytes(input, 0, input.length, output, 0)
      aesCtr.doFinal(output, 0)
      output
    }
  }
}
