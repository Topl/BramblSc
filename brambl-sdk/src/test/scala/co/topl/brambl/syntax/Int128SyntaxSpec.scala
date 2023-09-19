package co.topl.brambl.syntax

import co.topl.brambl.MockHelpers
import com.google.protobuf.ByteString
import quivr.models.Int128

class Int128SyntaxSpec extends munit.FunSuite with MockHelpers {

  val mockLong: Long = 100
  val mockBigInt: BigInt = BigInt(mockLong)
  val mockInt128: Int128 = Int128(ByteString.copyFrom(mockBigInt.toByteArray))

  test("int128AsBigInt") {
    assertEquals(mockInt128: BigInt, mockBigInt)
  }

  test("bigIntAsInt128") {
    assertEquals(mockBigInt: Int128, mockInt128)
  }

  test("longAsInt128") {
    assertEquals(mockLong: Int128, mockInt128)
  }
}
