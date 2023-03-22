package co.topl.crypto.utils

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

  def hexStringToStrictBytes(hexString: String): Array[Byte] = Hex.decode(hexString)

  object implicits {

    // will work on any string but the underlying decode implements a regex filter so the method should be safe enough for using in testing
    implicit class Ops(hexString: String) {
      def hexStringToBytes: Array[Byte] = decode(hexString)
    }
  }
}
