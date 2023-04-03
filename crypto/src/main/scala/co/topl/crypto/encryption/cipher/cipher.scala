package co.topl.crypto.encryption

import cats.Applicative
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

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

  object Codecs {

    implicit def cipherToJson[F[_]: Applicative]: Encoder[Cipher[F]] = new Encoder[Cipher[F]] {

      override def apply(a: Cipher[F]): Json =
        Json
          .obj("cipher" -> Json.fromString(a.params.cipher))
          .deepMerge(a match {
            case aes: Aes.AesParams => Aes.Codecs.aesParamsToJson(aes)
            case _                  => Json.Null
          })
    }

    implicit def cipherFromJson[F[_]: Applicative]: Decoder[Cipher[F]] = new Decoder[Cipher[F]] {

      override def apply(c: HCursor): Decoder.Result[Cipher[F]] =
        c.downField("cipher").as[String] match {
          case Right("aes") => Aes.Codecs.aesParamsFromJson(c).map(Aes.make[F](_))
          case _            => Left(DecodingFailure("Unknown Cipher", c.history))
        }
    }
  }
}
