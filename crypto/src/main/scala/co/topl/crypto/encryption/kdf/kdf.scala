package co.topl.crypto.encryption

import io.circe.Json

/**
 * Key derivation functions (KDFs) are used to derive a key from a password or passphrase.
 * @see [[https://en.wikipedia.org/wiki/Key_derivation_function]]
 */
package object kdf {

  /**
   * KDF parameters.
   */
  trait Params[F[_]] {
    // Label denoting which KDF to use
    val kdf: String
    def asJson: Json
  }

  /**
   * A KDF.
   */
  trait Kdf[F[_], P <: Params[F]] {

    /**
     * Derive a key from a secret.
     * @param secret secret to derive key from
     * @param params KDF parameters
     * @return derived key
     */
    def deriveKey(secret: Array[Byte], params: P): F[Array[Byte]]
  }
}
