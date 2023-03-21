package co.topl.crypto.utils

import cats.implicits.catsSyntaxOptionId

object Hex {

  /**
   * Encodes a byte array into a Base16 hex string
   * @param bytes - byte array to encode
   * @return - the encoded hex string
   */
  def encode(bytes: Array[Byte]): String =
    bytes.map("%02X" format _).mkString.toLowerCase

  /**
   * Decodes a Base16 hex string into a byte array
   * @param hexString - hex string to decode
   * @return - the decoded byte array
   */
  def decode(hexString: String): Array[Byte] =
    hexString.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)

  /**
   * Checks if a string is a valid Base16 hex string
   * @param hexString - string to check
   * @return - true if the string is a valid hex string, false otherwise
   */
  def isValidHex(hexString: String): Boolean = hexString.matches("^[0-9A-Fa-f]+$")

  /**
   * Converts a hex string to a byte array. Returns None if the string is not a valid hex string
   * @param hexString - hex string to convert
   * @return - the byte array if the string is a valid hex string, None otherwise
   */
  def hexStringToStrictBytes(hexString: String): Option[Array[Byte]] =
    if (isValidHex(hexString))
      decode(hexString).some
    else
      None

  object implicits {

    // will work on any string but the underlying decode implements a regex filter so the method should be safe enough for using in testing
    implicit class Ops(hexString: String) {
      def hexStringToBytes: Array[Byte] = decode(hexString)
    }
  }
}
