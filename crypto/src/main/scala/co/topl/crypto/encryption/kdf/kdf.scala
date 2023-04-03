package co.topl.crypto.encryption

import cats.Applicative
import io.circe.Json

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
    def asJson[F[_]: Applicative]: F[Json]
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
}
