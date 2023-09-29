package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.syntax.{bigIntAsInt128, valueToQuantitySyntaxOps}

class AggregationOpsSpec extends TransactionBuilderInterpreterSpecBase {

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
}
