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
   * Validate the MAC against a provided, expected, MAC.
   *
   * The main use case for this is to verify the integrity of decrypting a VaultStore. If the wrong password was
   * supplied during decryption, the MAC will not match the expectedMac (stored in the VaultStore).
   *
   * @param expectedMac the expected MAC value
   * @return true if this MAC matches the expectedMac, false otherwise
   */
  def validateMac[F[_]: Applicative](expectedMac: Array[Byte]): F[Boolean]

  /**
   * Validate the value of the MAC against a provided, expected, MAC value.
   *
   * The main use case for this is to verify the integrity of decrypting a VaultStore. If the wrong password was
   * supplied during decryption, the MAC will not match the expectedMac (stored in the VaultStore).
   *
   * @param expectedMacValue the expected MAC value
   * @return true if the MAC value matches the expectedMacValue, false otherwise
   */
  def validateMac[F[_]: Applicative](expectedMacValue: Mac): F[Boolean]
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
