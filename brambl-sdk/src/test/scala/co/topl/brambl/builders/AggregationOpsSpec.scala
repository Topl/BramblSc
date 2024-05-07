package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.syntax.{bigIntAsInt128, valueToQuantitySyntaxOps, valueToTypeIdentifierSyntaxOps}

class AggregationOpsSpec extends TransactionBuilderInterpreterSpecBase {

  test("typeIdentifier grouping") {
    val testMap = mockValues.groupBy(_.value.typeIdentifier)
    val expectedMap = Map(
      lvlValue.value.typeIdentifier            -> Seq(lvlValue, lvlValue.copy()),
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
      assetGroupSeriesAccumulatorAlt.value.typeIdentifier -> Seq(assetGroupSeriesAccumulatorAlt),
      assetGroupAccumulator.value.typeIdentifier -> Seq(
        assetGroupAccumulator,
        assetGroupAccumulator.copy()
      ),
      assetGroupAccumulatorAlt.value.typeIdentifier -> Seq(assetGroupAccumulatorAlt),
      assetSeriesAccumulator.value.typeIdentifier -> Seq(
        assetSeriesAccumulator,
        assetSeriesAccumulator.copy()
      ),
      assetSeriesAccumulatorAlt.value.typeIdentifier -> Seq(assetSeriesAccumulatorAlt),
      assetGroupSeriesFractionable.value.typeIdentifier -> Seq(
        assetGroupSeriesFractionable,
        assetGroupSeriesFractionable.copy()
      ),
      assetGroupSeriesFractionableAlt.value.typeIdentifier -> Seq(assetGroupSeriesFractionableAlt),
      assetGroupFractionable.value.typeIdentifier -> Seq(
        assetGroupFractionable,
        assetGroupFractionable.copy()
      ),
      assetGroupFractionableAlt.value.typeIdentifier -> Seq(assetGroupFractionableAlt),
      assetSeriesFractionable.value.typeIdentifier -> Seq(
        assetSeriesFractionable,
        assetSeriesFractionable.copy()
      ),
      assetSeriesFractionableAlt.value.typeIdentifier -> Seq(assetSeriesFractionableAlt),
      assetGroupSeriesImmutable.value.typeIdentifier -> Seq(
        assetGroupSeriesImmutable,
        assetGroupSeriesImmutable.copy()
      ),
      assetGroupSeriesImmutableAlt.value.typeIdentifier -> Seq(assetGroupSeriesImmutableAlt),
      assetGroupImmutable.value.typeIdentifier -> Seq(
        assetGroupImmutable,
        assetGroupImmutable.copy()
      ),
      assetGroupImmutableAlt.value.typeIdentifier -> Seq(assetGroupImmutableAlt),
      assetSeriesImmutable.value.typeIdentifier -> Seq(
        assetSeriesImmutable,
        assetSeriesImmutable.copy()
      ),
      assetSeriesImmutableAlt.value.typeIdentifier -> Seq(assetSeriesImmutableAlt),
      toplValue.value.typeIdentifier               -> Seq(toplValue, toplValue.copy()),
      toplReg1.value.typeIdentifier                -> Seq(toplReg1),
      toplReg2.value.typeIdentifier                -> Seq(toplReg2)
    )
    assertEquals(testMap, expectedMap)
  }

  test("DefaultAggregationOps.aggregate > different types") {
    val input = Seq(groupValue, groupValue, seriesValue, seriesValue).map(_.value)
    val testValues = DefaultAggregationOps.aggregate(input)
    assertEquals(testValues, input)
  }

  test("DefaultAggregationOps.aggregate > liquid assets") {
    val input = Seq(assetGroupSeries, assetGroupSeries, assetGroupSeries).map(_.value)
    val testValues = DefaultAggregationOps.aggregate(input)
    assertEquals(testValues, Seq(assetGroupSeries.value.setQuantity(3: BigInt)))
  }

  test("DefaultAggregationOps.aggregate > accumulator assets") {
    val input = Seq(assetGroupSeriesAccumulator, assetGroupSeriesAccumulator, assetGroupSeriesAccumulator).map(_.value)
    val testValues = DefaultAggregationOps.aggregate(input)
    assertEquals(testValues, input)
  }

  test("DefaultAggregationOps.aggregate > group tokens") {
    val input = Seq(groupValue, groupValue, groupValue).map(_.value)
    val testValues = DefaultAggregationOps.aggregate(input)
    assertEquals(testValues, Seq(groupValue.value.setQuantity(3: BigInt)))
  }

  test("DefaultAggregationOps.aggregate > topl tokens (no staking reg)") {
    val input = Seq(toplValue, toplValue).map(_.value)
    val testValues = DefaultAggregationOps.aggregate(input)
    assertEquals(testValues, Seq(toplValue.value.setQuantity(2: BigInt)))
  }

  test("DefaultAggregationOps.aggregate > topl tokens (with staking reg)") {
    val input = Seq(toplReg1, toplReg1).map(_.value)
    val testValues = DefaultAggregationOps.aggregate(input)
    assertEquals(testValues, input)
  }

  test("DefaultAggregationOps.aggregateWithChange > amount unspecified, liquid assets") {
    val input = Seq(assetGroupSeries, assetGroupSeries, assetGroupSeries).map(_.value)
    val testValues = DefaultAggregationOps.aggregateWithChange(input, None)
    assertEquals(testValues, (Seq(assetGroupSeries.value.setQuantity(3: BigInt)), Seq.empty))
  }

  test("DefaultAggregationOps.aggregateWithChange > amount unspecified, accumulator assets") {
    val input = Seq(assetGroupSeriesAccumulator, assetGroupSeriesAccumulator, assetGroupSeriesAccumulator).map(_.value)
    val testValues = DefaultAggregationOps.aggregateWithChange(input, None)
    assertEquals(testValues, (input, Seq.empty))
  }

  test("DefaultAggregationOps.aggregateWithChange > amount specified, accumulator assets") {
    val input = Seq(assetGroupSeriesAccumulator, assetGroupSeriesAccumulator, assetGroupSeriesAccumulator).map(_.value)
    val testValues = DefaultAggregationOps.aggregateWithChange(input, BigInt(2).some)
    assertEquals(testValues, (input, Seq.empty))
  }

  test("DefaultAggregationOps.aggregateWithChange > amount specified, liquid assets, amount >= quantity") {
    val input = Seq(assetGroupSeries, assetGroupSeries, assetGroupSeries).map(_.value)
    val testValues = DefaultAggregationOps.aggregateWithChange(input, BigInt(3).some)
    assertEquals(testValues, (Seq(assetGroupSeries.value.setQuantity(3: BigInt)), Seq.empty))
  }

  test("DefaultAggregationOps.aggregateWithChange > amount specified, liquid assets, amount < quantity") {
    val input = Seq(assetGroupSeries, assetGroupSeries, assetGroupSeries).map(_.value)
    val testValues = DefaultAggregationOps.aggregateWithChange(input, BigInt(2).some)
    assertEquals(
      testValues,
      (Seq(assetGroupSeries.value.setQuantity(2: BigInt)), Seq(assetGroupSeries.value.setQuantity(1: BigInt)))
    )
  }

  test("DefaultAggregationOps.aggregateWithChange > amount specified, Topls (no staking reg), amount < quantity") {
    val input = Seq(toplValue, toplValue, toplValue).map(_.value)
    val testValues = DefaultAggregationOps.aggregateWithChange(input, BigInt(2).some)
    assertEquals(
      testValues,
      (Seq(toplValue.value.setQuantity(2: BigInt)), Seq(toplValue.value.setQuantity(1: BigInt)))
    )
  }

  test("DefaultAggregationOps.aggregateWithChange > amount specified, Topls (with staking reg), amount < quantity") {
    val input = Seq(toplReg1, toplReg1, toplReg1).map(_.value)
    val testValues = DefaultAggregationOps.aggregateWithChange(input, BigInt(2).some)
    assertEquals(
      testValues,
      (input, Seq.empty)
    )
  }
}
