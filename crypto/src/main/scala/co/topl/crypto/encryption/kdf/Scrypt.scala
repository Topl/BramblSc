package co.topl.crypto.encryption.kdf

import org.bouncycastle.crypto.generators.SCrypt

/**
 * Scrypt parameters.
 *
 * @param salt  salt
 * @param n     CPU/Memory cost parameter.
 *              Must be larger than 1, a power of 2 and less than 2^(128 * r / 8)^. Defaults to 2^18^.
 * @param r     the block size.
 *              Must be &gt;= 1. Defaults to 8.
 * @param p     Parallelization parameter.
 *              Must be a positive integer less than or equal to Integer.MAX_VALUE / (128 * r * 8). Defaults to 1.
 * @param dkLen length of derived key. Defaults to 32.
 */
case class ScryptParams(
  salt:  Array[Byte],
  n:     Int = scala.math.pow(2, 18).toInt,
  r:     Int = 8,
  p:     Int = 1,
  dkLen: Int = 32
) extends Params

/**
 * Scrypt is a key derivation function.
 * @see [[https://en.wikipedia.org/wiki/Scrypt]]
 */
object Scrypt extends Kdf[ScryptParams] {

  /**
   * Generate a random salt.
   *
   * @return a random salt of 32 bytes
   */
  def generateSalt: Array[Byte] = {
    val salt = new Array[Byte](32)
    new java.util.Random().nextBytes(salt)
    salt
  }

  /**
   * Derive a key from a secret.
   * @param secret secret to derive key from
   * @param params KDF parameters
   * @return derived key
   */
  override def deriveKey(secret: Array[Byte], params: ScryptParams): Array[Byte] =
    SCrypt.generate(secret, params.salt, params.n, params.r, params.p, params.dkLen)
}
