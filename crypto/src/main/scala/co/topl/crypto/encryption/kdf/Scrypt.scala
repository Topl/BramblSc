package co.topl.crypto.encryption.kdf

import org.bouncycastle.crypto.generators.SCrypt

/**
 * Scrypt is a key derivation function.
 * @see [[https://en.wikipedia.org/wiki/Scrypt]]
 */
class Scrypt extends Kdf[Scrypt.ScryptParams] {
  /**
   * Derive a key from a secret.
   * @param secret secret to derive key from
   * @param params KDF parameters
   * @return derived key
   */
  override def deriveKey(secret: Array[Byte], params: Scrypt.ScryptParams): Array[Byte] =
    SCrypt.generate(secret, params.salt, params.n, params.r, params.p, params.dkLen)
}

object Scrypt {
  /**
   * Scrypt parameters.
   */
  trait ScryptParams extends Params {
    // salt
    val salt : Array[Byte]
    // CPU/Memory cost parameter. Must be larger than 1, a power of 2 and less than 2^(128 * r / 8)
    val n: Int = scala.math.pow(2, 18).toInt
    // the block size, must be >= 1.
    val r: Int = 8
    // Parallelization parameter. Must be a positive integer less than or equal to Integer.MAX_VALUE / (128 * r * 8).
    val p: Int = 1
    // length of derived key
    val dkLen: Int = 32
  }

  /**
   * Generate a random salt.
   * @return a random salt
   */
  def generateSalt: Array[Byte] = {
    val salt = new Array[Byte](32)
    new java.util.Random().nextBytes(salt)
    salt
  }
}
