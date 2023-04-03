package co.topl.crypto.encryption.kdf

import cats.Applicative
import cats.implicits.catsSyntaxApplicativeId
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.bouncycastle.crypto.generators.{SCrypt => SCryptImpl}
import org.bouncycastle.util.Strings

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
  ) extends Params { override val kdf: String = "scrypt" }

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

  object Codecs {

    implicit val sCryptParamsToJson: Encoder[SCryptParams] = new Encoder[SCryptParams] {

      override def apply(a: SCryptParams): Json = Json.obj(
        "salt"  -> Json.fromString(Strings.fromByteArray(a.salt)),
        "n"     -> Json.fromInt(a.n),
        "r"     -> Json.fromInt(a.r),
        "p"     -> Json.fromInt(a.p),
        "dkLen" -> Json.fromInt(a.dkLen)
      )
    }

    implicit val sCryptParamsFromJson: Decoder[SCryptParams] = new Decoder[SCryptParams] {

      override def apply(c: HCursor): Decoder.Result[SCryptParams] = for {
        salt  <- c.downField("salt").as[String]
        n     <- c.downField("n").as[Int]
        r     <- c.downField("r").as[Int]
        p     <- c.downField("p").as[Int]
        dkLen <- c.downField("dkLen").as[Int]
      } yield SCryptParams(salt = Strings.toByteArray(salt), n = n, r = r, p = p, dkLen = dkLen)
    }
  }
}
