package co.topl.brambl.syntax

import com.google.protobuf.ByteString
import quivr.models.Int128

import scala.language.implicitConversions

trait Int128Syntax {
  implicit def int128AsBigInt(int128: Int128): BigInt = BigInt(int128.value.toByteArray)
  implicit def bigIntAsInt128(bigInt: BigInt): Int128 = Int128(ByteString.copyFrom(bigInt.toByteArray))
  implicit def longAsInt128(long:     Long): Int128 = BigInt(long)
}
