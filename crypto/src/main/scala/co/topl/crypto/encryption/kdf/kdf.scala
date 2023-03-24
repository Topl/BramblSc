package co.topl.crypto.encryption

/**
 * Key derivation functions (KDFs) are used to derive a key from a password or passphrase.
 * @see [[https://en.wikipedia.org/wiki/Key_derivation_function]]
 */
package object kdf {
  /**
   * KDF parameters.
   */
  trait Params
  /**
   * A KDF.
   */
  trait Kdf[P <: Params] {
    /**
     * Derive a key from a secret.
     * @param secret secret to derive key from
     * @param params KDF parameters
     * @return derived key
     */
    def deriveKey(secret: Array[Byte], params: P): Array[Byte]
  }
}
