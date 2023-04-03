package co.topl.crypto.encryption

import cats.Applicative
import cats.implicits.{catsSyntaxApplicativeId, toFunctorOps}
import co.topl.crypto.hash

/**
 * Message authentication codes (MACs) are used to verify the integrity of data.
 *
 * @see [[https://en.wikipedia.org/wiki/Message_authentication_code]]
 */
trait Mac[F[_]] {
  // The MAC
  val value: F[Array[Byte]]
  def validateMac(expectedMac: Array[Byte]): F[Boolean]
}

object Mac {

  /**
   * Create MAC for a KeyFile.
   * The KeyFile MAC is used to verify the integrity of the cipher text and derived key.
   * It is calculated by hashing the last 16 bytes of the derived key + cipher text
   *
   * @param derivedKey the derived key
   * @param cipherText the cipher text
   * @return MAC
   */
  def make[F[_]: Applicative](derivedKey: Array[Byte], cipherText: Array[Byte]): Mac[F] = new Mac[F] {
    private def calculateMac(data: Array[Byte]): Array[Byte] = hash.blake2b256.hash(data).value

    override val value: F[Array[Byte]] = calculateMac(derivedKey.takeRight(16) ++ cipherText).pure[F]

    override def validateMac(expectedMac: Array[Byte]): F[Boolean] = value.map(java.util.Arrays.equals(_, expectedMac))
  }
}
