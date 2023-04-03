package co.topl.crypto.encryption

import co.topl.crypto.hash

/**
 * Message authentication codes (MACs) are used to verify the integrity of data.
 *
 * @see [[https://en.wikipedia.org/wiki/Message_authentication_code]]
 */
object Mac {
  private def calculateMac(data: Array[Byte]): Array[Byte] = hash.blake2b256.hash(data).value

  /**
   * Calculate the MAC for a KeyFile.
   * The KeyFile MAC is used to verify the integrity of the cipher text and derived key.
   *
   * It is calculated by hashing the last 16 bytes of the derived key + cipher text
   *
   * @param derivedKey the derived key
   * @param cipherText the cipher text
   * @return MAC
   */
  def calculateKeyFileMac(derivedKey: Array[Byte], cipherText: Array[Byte]): Array[Byte] =
    calculateMac(derivedKey.takeRight(16) ++ cipherText)

  /**
   * Validate the MAC for a KeyFile.
   *
   * @param derivedKey the derived key
   * @param cipherText the cipher text
   * @param mac the MAC to validate
   * @return true if the MAC is valid, false otherwise
   */
  def validateKeyFileMac(derivedKey: Array[Byte], cipherText: Array[Byte], mac: Array[Byte]): Boolean =
    java.util.Arrays.equals(calculateKeyFileMac(derivedKey, cipherText), mac)
}
