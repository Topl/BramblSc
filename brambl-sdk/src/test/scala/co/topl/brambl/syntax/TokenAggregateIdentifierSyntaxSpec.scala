package co.topl.brambl.syntax

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.{Value => BoxValue}
import com.google.protobuf.ByteString
import quivr.models.Int128

class TokenAggregateIdentifierSyntaxSpec extends munit.FunSuite with MockHelpers {

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
    // TODO: From here
    assetGroup,
    assetGroup.copy(), // exact duplicate
    assetGroupAlt, // diff group and series
    assetSeries,
    assetSeries.copy(), // exact duplicate
    assetSeriesAlt, // diff group and series
    assetGroupSeriesImmutable,
    assetGroupSeriesImmutable.copy(), // exact duplicate
    assetGroupSeriesFractionable,
    assetGroupSeriesFractionable.copy(), // exact duplicate
    assetGroupSeriesAccumulator,
    assetGroupSeriesAccumulator.copy() // exact duplicate
  )

  // TODO: Finish
  test("typeIdentifier grouping".fail) {
    val testMap = mockValues.groupBy(_.value.typeIdentifier.aggregateIdentifier)
    val expectedMap = Map(
      value.value.typeIdentifier.aggregateIdentifier               -> Seq(value, value.copy()),
      groupValue.value.typeIdentifier.aggregateIdentifier          -> Seq(groupValue, groupValue.copy()),
      groupValueAlt.value.typeIdentifier.aggregateIdentifier       -> Seq(groupValueAlt),
      seriesValue.value.typeIdentifier.aggregateIdentifier         -> Seq(seriesValue, seriesValue.copy()),
      seriesValueAlt.value.typeIdentifier.aggregateIdentifier      -> Seq(seriesValueAlt),
      assetGroupSeries.value.typeIdentifier.aggregateIdentifier    -> Seq(assetGroupSeries, assetGroupSeries.copy()),
      assetGroupSeriesAlt.value.typeIdentifier.aggregateIdentifier -> Seq(assetGroupSeriesAlt)
      // from here
    )
    assertEquals(testMap, expectedMap)
  }
}
