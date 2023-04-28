package co.topl.brambl.utils

import co.topl.crypto.hash.sha256

import scala.util.Failure
import scala.util.Success
import scala.util.Try

sealed abstract trait EncodingError extends Exception
case object InvalidChecksum extends EncodingError
case object InvalidInputString extends EncodingError

object Encoding {

  val base58alphabet =
    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".zipWithIndex

  val idxToCharBase58 = Map(base58alphabet.map(_.swap): _*)

  val charToIdxBase58 = Map(base58alphabet: _*)

  /**
   * Encodes a byte array into a base58 string
   * @param array
   *   the byte array to encode
   * @return
   *   the base58 string
   */
  def encodeToBase58(array: Array[Byte]): String =
    (LazyList.fill(array.takeWhile(_ == 0).length)(1.toByte) ++ LazyList
      .unfold(
        BigInt(0.toByte +: array)
      )(n => if (n == 0) None else Some((n /% 58).swap)) // we decompose it into base58 digits
      .map(_.toInt)
      .reverse
      .map(x => idxToCharBase58(x))).mkString

  /**
   * Encodes a byte array into a hex string.
   * @param array
   *   the byte array to encode
   * @return
   *   the hex string
   */
  def encodeToHex(array: Array[Byte]): String =
    array.map("%02x".format(_)).mkString

  /**
   * Decodes a hex string into a byte array.
   * @param hex
   *   the hex string to decode
   * @return
   *   the byte array
   */
  def decodeFromHex(hex: String): Either[EncodingError, Array[Byte]] =
    Try(hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray) match {
      case Success(value) => Right(value)
      case Failure(_)     => Left(InvalidInputString)
    }

  /**
   * Decodes a base58 string into a byte array.
   * @param b58
   *   the base58 string to decode
   * @return
   *   the byte array
   */
  def decodeFromBase58(b58: String): Either[EncodingError, Array[Byte]] =
    Try({
      val zeroCount = b58.takeWhile(_ == '1').length
      Array.fill(zeroCount)(0.toByte) ++
      b58
        .drop(zeroCount)
        .map(charToIdxBase58)
        .toList
        .foldLeft(BigInt(0))((acc, x) => acc * 58 + x)
        .toByteArray
        .dropWhile(_ == 0.toByte)
    }) match {
      case Success(value) => Right(value)
      case Failure(_)     => Left(InvalidInputString)
    }

  /**
   * Encodes a byte array into a base58 string with a checksum.
   * @param payload
   *   the byte array to encode
   * @return
   *   the base58 string
   */
  def encodeToBase58Check(payload: Array[Byte]): String = {
    val checksum =
      sha256.hash(sha256.hash(payload).value).value.take(4)
    encodeToBase58(payload.concat(checksum))
  }

  /**
   * Decodes a base58 string with a checksum into a byte array.
   *
   * @param b58
   *   the base58 string to decode
   * @return
   *   the byte array
   */
  def decodeFromBase58Check(b58: String): Either[EncodingError, Array[Byte]] =
    for {
      decoded <- decodeFromBase58(b58)
      (payload, errorCheckingCode) = decoded.splitAt(decoded.length - 4)
      expectedErrorCheckingCode =
        sha256.hash(sha256.hash(payload).value).value.take(4)
      result <- Either.cond(
        errorCheckingCode.sameElements(expectedErrorCheckingCode),
        payload,
        InvalidChecksum
      )
    } yield result

}
