package co.topl.models.utility

import co.topl.models.{Bytes, TypedBytes}

import scala.annotation.unused

object Sized {

  // noinspection ScalaUnusedSymbol
  class Strict[Data, Length] private[Sized] (val data: Data) extends AnyVal {
    type L = Length

    override def toString: String = s"Sized.Strict(${data.toString})"
  }

  class Max[Data, Length] private[Sized] (val data: Data) extends AnyVal {
    type L = Length

    override def toString: String = s"Sized.Max(${data.toString})"
  }

  def strict[Data: HasLength, L <: Length](data: Data)(implicit length: L): Either[InvalidLength, Strict[Data, L]] = {
    val dataLength = implicitly[HasLength[Data]].length(data)
    Either.cond(
      dataLength == length.value,
      new Strict(data),
      InvalidLength(dataLength)
    )
  }

  def strictUnsafe[Data: HasLength, L <: Length](data: Data)(implicit length: L): Strict[Data, L] = {
    val dataLength = implicitly[HasLength[Data]].length(data)
    require(dataLength == length.value, s"data.length ($dataLength) did not equal target length (${length.value})")
    new Strict(data)
  }

  def max[Data: HasLength, L <: Length](data: Data)(implicit length: L): Either[InvalidLength, Max[Data, L]] = {
    val dataLength = implicitly[HasLength[Data]].length(data)
    Either.cond(
      dataLength <= length.value,
      new Max(data),
      InvalidLength(dataLength)
    )
  }

  @unused
  def maxUnsafe[Data: HasLength, L <: Length](data: Data)(implicit length: L): Max[Data, L] = {
    val dataLength = implicitly[HasLength[Data]].length(data)
    require(dataLength <= length.value)
    new Max(data)
  }

  case class InvalidLength(length: Int)
}

sealed abstract class Length(val value: Int)

object Lengths {
  implicit case object Empty extends Length(0)
  implicit case object bytes1 extends Length(1)
  implicit case object bytes2 extends Length(2)
  implicit case object bytes4 extends Length(4)
  implicit case object bytes8 extends Length(8)
  implicit case object bytes32 extends Length(32)
  implicit case object bytes33 extends Length(33)
  implicit case object bytes58 extends Length(58)
  implicit case object bytes64 extends Length(64)
  implicit case object bytes80 extends Length(80)
  implicit case object bytes96 extends Length(96)
  implicit case object bytes127 extends Length(127)
  implicit case object bytes128 extends Length(128)
  implicit case object bytes256 extends Length(256)
  implicit case object bytes704 extends Length(704)
  implicit case object bytes1448 extends Length(1448)
}

trait HasLength[T] {
  def length(t: T): Int
}

object HasLength {

  trait Instances {

    implicit def bytesLength: HasLength[Bytes] =
      _.length.toInt

    implicit def arrayLength[T]: HasLength[Array[T]] =
      _.length

    implicit val stringLength: HasLength[String] =
      _.length

    implicit val bigIntLength: HasLength[BigInt] =
      _.bitLength

    implicit val typedDataLength: HasLength[TypedBytes] =
      _.allBytes.length.toInt
  }

  object instances extends Instances

}
