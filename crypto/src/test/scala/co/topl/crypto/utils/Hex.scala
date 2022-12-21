package co.topl.crypto.utils

import scodec.bits.ByteVector

object Hex {

  def encode(bytes: Array[Byte]): String =
    bytes.map("%02X" format _).mkString.toLowerCase

  def encode(bytes: ByteVector): String = encode(bytes.toArray)

  def decode(hexString: String): Array[Byte] =
    hexString.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)

  def hexStringToStrictBytes(hexString: String): ByteVector =
    ByteVector(Hex.decode(hexString))

  object implicits {

    // will work on any string but the underlying decode implements a regex filter so the method should be safe enough for using in testing
    implicit class Ops(hexString: String) {
      def hexStringToBytes: ByteVector = ByteVector(decode(hexString))
    }
  }
}
