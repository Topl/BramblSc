package co.topl.crypto.encryption

import cats.Applicative
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

/**
 * Key derivation functions (KDFs) are used to derive a key from a password or passphrase.
 * @see [[https://en.wikipedia.org/wiki/Key_derivation_function]]
 */
package object kdf {

  /**
   * KDF parameters.
   */
  trait Params {
    // Label denoting which KDF to use
    val kdf: String
  }

  /**
   * A KDF.
   */
  trait Kdf[F[_]] {
    // KDF parameters
    val params: Params

    /**
     * Derive a key from a secret.
     * @param secret secret to derive key from
     * @return derived key
     */
    def deriveKey(secret: Array[Byte]): F[Array[Byte]]
  }

  /**
   * JSON codecs for a KDF
   */
  object Codecs {

    /**
     * KDF JSON encoder
     */
    implicit def kdfToJson[F[_]: Applicative]: Encoder[Kdf[F]] = new Encoder[Kdf[F]] {

      override def apply(a: Kdf[F]): Json =
        Json
          .obj("kdf" -> Json.fromString(a.params.kdf))
          .deepMerge(a.params match {
            case sCrypt: SCrypt.SCryptParams => SCrypt.Codecs.sCryptParamsToJson(sCrypt)
            case _                           => Json.Null
          })
    }

    /**
     * KDF JSON decoder
     */
    implicit def kdfFromJson[F[_]: Applicative]: Decoder[Kdf[F]] = new Decoder[Kdf[F]] {

      override def apply(c: HCursor): Decoder.Result[Kdf[F]] =
        c.downField("kdf").as[String] match {
          case Right("scrypt") => SCrypt.Codecs.sCryptParamsFromJson(c).map(SCrypt.make[F](_))
          case _               => Left(DecodingFailure("Unknown KDF", c.history))
        }
    }
  }
}
