package co.topl.crypto.encryption

/**
 * Key derivation functions (KDFs) are used to derive a key from a password or passphrase.
 * @see [[https://en.wikipedia.org/wiki/Key_derivation_function]]
 */
package object kdf {
  trait Params
  trait Kdf[P <: Params] {
    def deriveKey(secret: Array[Byte], params: P): Array[Byte]
  }
}
