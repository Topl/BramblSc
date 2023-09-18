package co.topl.brambl.syntax

import co.topl.brambl.MockHelpers
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.{Value => BoxValue}
import com.google.protobuf.ByteString
import quivr.models.Int128

class BoxValueSyntaxSpec extends munit.FunSuite with MockHelpers {

  val mockNewQuantity: Int128 = Int128(ByteString.copyFrom(BigInt(100).toByteArray))

  test("lvlAsBoxVal") {
    assertEquals(value.getLvl: BoxValue, value.value)
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
    assertEquals(value.value.quantity, quantity)
    assertEquals(groupValue.value.quantity, quantity)
    assertEquals(seriesValue.value.quantity, quantity)
    assertEquals(assetGroupSeries.value.quantity, quantity)
    intercept[Exception](BoxValue.Topl(Value.TOPL(mockNewQuantity)).quantity)
  }

  test("setQuantity") {
    assertEquals(value.value.setQuantity(mockNewQuantity).quantity, mockNewQuantity)
    assertEquals(groupValue.value.setQuantity(mockNewQuantity).quantity, mockNewQuantity)
    assertEquals(seriesValue.value.setQuantity(mockNewQuantity).quantity, mockNewQuantity)
    assertEquals(assetGroupSeries.value.setQuantity(mockNewQuantity).quantity, mockNewQuantity)
    intercept[Exception](BoxValue.Topl(Value.TOPL(mockNewQuantity)).setQuantity(mockNewQuantity))
  }

  test("typeIdentifier") {
    assertEquals(value.value.typeIdentifier, LvlType)
    assertEquals(groupValue.value.typeIdentifier, GroupType(mockGroupPolicy.computeId))
    assertEquals(seriesValue.value.typeIdentifier, SeriesType(mockSeriesPolicy.computeId))
    assertEquals(
      assetGroupSeries.value.typeIdentifier,
      GroupAndSeriesFungible(mockGroupPolicy.computeId, mockSeriesPolicy.computeId)
    )
    assertEquals(assetGroup.value.typeIdentifier, GroupFungible(mockGroupPolicy.computeId))
    assertEquals(assetSeries.value.typeIdentifier, SeriesFungible(mockSeriesPolicy.computeId))
    intercept[Exception](BoxValue.Topl(Value.TOPL(mockNewQuantity)).typeIdentifier)
  }
}
