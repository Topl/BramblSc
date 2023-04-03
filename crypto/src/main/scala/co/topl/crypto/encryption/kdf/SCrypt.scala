package co.topl.crypto.encryption.kdf

import cats.Applicative
import cats.implicits.catsSyntaxApplicativeId
import io.circe.Json
import org.bouncycastle.crypto.generators.{SCrypt => SCryptImpl}

/**
 * Scrypt is a key derivation function.
 * @see [[https://en.wikipedia.org/wiki/Scrypt]]
 */
object SCrypt {

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
  case class SCryptParams(
    salt:  Array[Byte],
    n:     Int = scala.math.pow(2, 18).toInt,
    r:     Int = 8,
    p:     Int = 1,
    dkLen: Int = 32
  ) extends Params {
    override val kdf: String = "scrypt"

    override def asJson[F[_]: Applicative]: F[Json] = Json
      .obj(
        "salt"  -> Json.fromString(salt.map("%02x" format _).mkString),
        "n"     -> Json.fromInt(n),
        "r"     -> Json.fromInt(r),
        "p"     -> Json.fromInt(p),
        "dkLen" -> Json.fromInt(dkLen),
        "kdf"   -> Json.fromString(kdf)
      )
      .pure[F]
  }

  def make[F[_]: Applicative](sCryptParams: SCryptParams): Kdf[F] = new Kdf[F] {
    override val params: SCryptParams = sCryptParams

    /**
     * Derive a key from a secret.
     *
     * @param secret secret to derive key from
     * @return derived key
     */
    override def deriveKey(secret: Array[Byte]): F[Array[Byte]] =
      SCryptImpl.generate(secret, params.salt, params.n, params.r, params.p, params.dkLen).pure[F]
  }
}
