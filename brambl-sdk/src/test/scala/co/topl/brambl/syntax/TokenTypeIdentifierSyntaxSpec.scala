package co.topl.brambl.syntax

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.{Value => BoxValue}
import com.google.protobuf.ByteString
import quivr.models.Int128

class TokenTypeIdentifierSyntaxSpec extends munit.FunSuite with MockHelpers {

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
    intercept[Exception](BoxValue.Topl(Value.TOPL(quantity)).typeIdentifier)
  }

  val mockNewQuantity: Int128 = Int128(ByteString.copyFrom(BigInt(100).toByteArray))

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

  private val assetGroupAlt = assetGroup.copy(
    assetGroup.getAsset.copy(
      groupId = mockGroupPolicyAlt.computeId.some,
      seriesId = mockSeriesPolicyAlt.computeId.some
    )
  )

  private val assetSeriesAlt = assetSeries.copy(
    assetSeries.getAsset.copy(
      groupId = mockGroupPolicyAlt.computeId.some,
      seriesId = mockSeriesPolicyAlt.computeId.some
    )
  )

  private val assetGroupSeriesAccumulatorAlt = assetGroupSeriesAccumulator.copy(
    assetGroupSeriesAccumulator.getAsset.copy(
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
    assetGroupSeriesAlt, // diff group and series
    assetGroup,
    assetGroup.copy(), // exact duplicate
    assetGroupAlt, // diff group and series
    assetSeries,
    assetSeries.copy(), // exact duplicate
    assetSeriesAlt, // diff group and series
    assetGroupSeriesAccumulator,
    assetGroupSeriesAccumulator.copy(), // exact duplicate
    assetGroupSeriesAccumulatorAlt // diff group and series
  )

  test("typeIdentifier grouping") {
    val testMap = mockValues.groupBy(_.value.typeIdentifier)
    val expectedMap = Map(
      value.value.typeIdentifier               -> Seq(value, value.copy()),
      groupValue.value.typeIdentifier          -> Seq(groupValue, groupValue.copy()),
      groupValueAlt.value.typeIdentifier       -> Seq(groupValueAlt),
      seriesValue.value.typeIdentifier         -> Seq(seriesValue, seriesValue.copy()),
      seriesValueAlt.value.typeIdentifier      -> Seq(seriesValueAlt),
      assetGroupSeries.value.typeIdentifier    -> Seq(assetGroupSeries, assetGroupSeries.copy()),
      assetGroupSeriesAlt.value.typeIdentifier -> Seq(assetGroupSeriesAlt),
      assetGroup.value.typeIdentifier          -> Seq(assetGroup, assetGroup.copy()),
      assetGroupAlt.value.typeIdentifier       -> Seq(assetGroupAlt),
      assetSeries.value.typeIdentifier         -> Seq(assetSeries, assetSeries.copy()),
      assetSeriesAlt.value.typeIdentifier      -> Seq(assetSeriesAlt),
      assetGroupSeriesAccumulator.value.typeIdentifier -> Seq(
        assetGroupSeriesAccumulator,
        assetGroupSeriesAccumulator.copy()
      ),
      assetGroupSeriesAccumulatorAlt.value.typeIdentifier -> Seq(assetGroupSeriesAccumulatorAlt)
    )
    assertEquals(testMap, expectedMap)
  }
}
