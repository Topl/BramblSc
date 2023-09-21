package co.topl.brambl.syntax

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
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
    val gId = mockGroupPolicy.computeId
    val sId = mockSeriesPolicy.computeId
    val qd = mockSeriesPolicy.quantityDescriptor
    assertEquals(value.value.typeIdentifier, LvlType)
    assertEquals(groupValue.value.typeIdentifier, GroupType(gId))
    assertEquals(seriesValue.value.typeIdentifier, SeriesType(sId))
    assertEquals(assetGroupSeries.value.typeIdentifier, GroupAndSeriesFungible(gId, sId, qd))
    assertEquals(assetGroup.value.typeIdentifier, GroupFungible(gId, sId.value, qd))
    assertEquals(assetSeries.value.typeIdentifier, SeriesFungible(sId, gId.value, qd))
    val mockAlloy = ByteString.copyFrom(Array.fill(32)(0.toByte))
    val testAlloy = ByteString.copyFrom(Array.fill(32)(0.toByte))
    assertEquals(
      assetGroup.copy(assetGroup.getAsset.copy(seriesAlloy = mockAlloy.some)).value.typeIdentifier,
      GroupFungible(gId, testAlloy, qd)
    )
    assertEquals(
      assetSeries.copy(assetSeries.getAsset.copy(groupAlloy = mockAlloy.some)).value.typeIdentifier,
      SeriesFungible(sId, testAlloy, qd)
    )
    intercept[Exception](BoxValue.Topl(Value.TOPL(mockNewQuantity)).typeIdentifier)
  }
  private val mockSeriesPolicyAlt = SeriesPolicy("Mock Series Policy", None, dummyTxoAddress.copy(index = 44))
  private val mockGroupPolicyAlt = GroupPolicy("Mock Group Policy", dummyTxoAddress.copy(index = 55))

  private val groupValueAlt = groupValue.copy(groupValue.getGroup.copy(groupId = mockGroupPolicyAlt.computeId))
  private val seriesValueAlt = seriesValue.copy(seriesValue.getSeries.copy(seriesId = mockSeriesPolicyAlt.computeId))

  private val assetGroupSeriesAlt = assetGroupSeries.copy(
    assetGroupSeries.getAsset.copy(
      groupId = mockGroupPolicyAlt.computeId.some,
      seriesId = mockSeriesPolicyAlt.computeId.some
    )
  )

  private val mockValues = Seq(
    value,
    value.copy(), // exact duplicate
    groupValue,
    groupValue.copy(), // exact duplicate
    groupValueAlt, // diff group
    seriesValue,
    seriesValue.copy(), // exact duplicate
    seriesValueAlt, // diff series
    assetGroupSeries,
    assetGroupSeries.copy(), // exact duplicate
    assetGroupSeriesAlt // diff group and series
  )

  test("typeIdentifier grouping") {
    val testMap = mockValues.groupBy(_.value.typeIdentifier.aggregateIdentifier)
    val expectedMap = Map(
      value.value.typeIdentifier.aggregateIdentifier               -> Seq(value, value.copy()),
      groupValue.value.typeIdentifier.aggregateIdentifier          -> Seq(groupValue, groupValue.copy()),
      groupValueAlt.value.typeIdentifier.aggregateIdentifier       -> Seq(groupValueAlt),
      seriesValue.value.typeIdentifier.aggregateIdentifier         -> Seq(seriesValue, seriesValue.copy()),
      seriesValueAlt.value.typeIdentifier.aggregateIdentifier      -> Seq(seriesValueAlt),
      assetGroupSeries.value.typeIdentifier.aggregateIdentifier    -> Seq(assetGroupSeries, assetGroupSeries.copy()),
      assetGroupSeriesAlt.value.typeIdentifier.aggregateIdentifier -> Seq(assetGroupSeriesAlt)
    )
    assertEquals(testMap, expectedMap)
  }
}
