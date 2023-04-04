package co.topl.crypto.encryption

import cats.Applicative
import cats.implicits.catsSyntaxApplicativeId
import co.topl.crypto.hash

/**
 * Message authentication codes (MACs) are used to verify the integrity of data.
 *
 * @see [[https://en.wikipedia.org/wiki/Message_authentication_code]]
 */
trait Mac {
  // The MAC
  val value: Array[Byte]

  /**
   * Validate the MAC.
   *
   * @param expectedMac the expected MAC value
   * @return true if the MAC is valid, false otherwise
   */
  def validateMac[F[_]: Applicative](expectedMac: Array[Byte]): F[Boolean]

  /**
   * Validate the MAC.
   *
   * @param expectedMac the expected MAC
   * @return true if the MAC is valid, false otherwise
   */
  def validateMac[F[_]: Applicative](expectedMac: Mac): F[Boolean]
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
  def make(derivedKey: Array[Byte], cipherText: Array[Byte]): Mac = new Mac {
    private def calculateMac(data: Array[Byte]): Array[Byte] = hash.blake2b256.hash(data).value

    override val value: Array[Byte] = calculateMac(derivedKey.takeRight(16) ++ cipherText)

    override def validateMac[F[_]: Applicative](expectedMac: Array[Byte]): F[Boolean] =
      java.util.Arrays.equals(value, expectedMac).pure[F]

    override def validateMac[F[_]: Applicative](expectedMac: Mac): F[Boolean] = validateMac(expectedMac.value)
  }
}
