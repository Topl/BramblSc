package co.topl.brambl.syntax

import co.topl.brambl.MockHelpers
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.{Value => BoxValue}
import com.google.protobuf.ByteString
import quivr.models.Int128

class BoxValueSyntaxSpec extends munit.FunSuite with MockHelpers {

  val mockNewQuantity: Int128 = Int128(ByteString.copyFrom(BigInt(100).toByteArray))

  test("lvlAsBoxVal") {
    assertEquals(lvlValue.getLvl: BoxValue, lvlValue.value)
  }

  test("groupAsBoxVal") {
    assertEquals(groupValue.getGroup: BoxValue, groupValue.value)
  }

  test("seriesAsBoxVal") {
    assertEquals(seriesValue.getSeries: BoxValue, seriesValue.value)
  }

  test("assetAsBoxVal") {
    assertEquals(assetGroupSeries.getAsset: BoxValue, assetGroupSeries.value)
  }

  test("get quantity") {
    assertEquals(lvlValue.value.quantity, quantity)
    assertEquals(groupValue.value.quantity, quantity)
    assertEquals(seriesValue.value.quantity, quantity)
    assertEquals(assetGroupSeries.value.quantity, quantity)
    assertEquals(toplValue.value.quantity, quantity)
    intercept[Exception](Value.defaultInstance.value.quantity)
  }

  test("setQuantity") {
    assertEquals(lvlValue.value.setQuantity(mockNewQuantity).quantity, mockNewQuantity)
    assertEquals(groupValue.value.setQuantity(mockNewQuantity).quantity, mockNewQuantity)
    assertEquals(seriesValue.value.setQuantity(mockNewQuantity).quantity, mockNewQuantity)
    assertEquals(assetGroupSeries.value.setQuantity(mockNewQuantity).quantity, mockNewQuantity)
    assertEquals(toplValue.value.setQuantity(mockNewQuantity).quantity, mockNewQuantity)
    intercept[Exception](Value.defaultInstance.value.setQuantity(mockNewQuantity))
  }
}
