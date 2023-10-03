package co.topl.brambl.builders

import cats.Id
import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.MockHelpers
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances.valueImmutable
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{IoTransaction, UnspentTransactionOutput}
import co.topl.brambl.models.LockAddress
import co.topl.brambl.syntax.{
  assetAsBoxVal,
  groupAsBoxVal,
  groupPolicyAsGroupPolicySyntaxOps,
  seriesAsBoxVal,
  seriesPolicyAsSeriesPolicySyntaxOps
}
import co.topl.genus.services.Txo
import co.topl.genus.services.TxoState.UNSPENT

trait TransactionBuilderInterpreterSpecBase extends munit.FunSuite with MockHelpers {
  val txBuilder: TransactionBuilderApi[Id] = TransactionBuilderApi.make[Id](0, 0)

  def valToTxo(value: Value, lockAddr: LockAddress = inLockFullAddress): Txo =
    Txo(valToUtxo(value, lockAddr), UNSPENT, dummyTxoAddress)

  def valToUtxo(value: Value, lockAddr: LockAddress = inLockFullAddress): UnspentTransactionOutput =
    UnspentTransactionOutput(lockAddr, value)

  def sortedTx(tx: IoTransaction): IoTransaction = tx
    .withOutputs(tx.outputs.sortBy(_.value.immutable.value.toByteArray.mkString))
    .withInputs(tx.inputs.sortBy(_.value.immutable.value.toByteArray.mkString))

  val mockSeriesPolicyAlt: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, dummyTxoAddress.copy(index = 44))
  val mockGroupPolicyAlt: GroupPolicy = GroupPolicy("Mock Group Policy", dummyTxoAddress.copy(index = 55))

  val groupValueAlt: Value = groupValue.copy(groupValue.getGroup.copy(groupId = mockGroupPolicyAlt.computeId))

  val seriesValueAlt: Value = seriesValue.copy(seriesValue.getSeries.copy(seriesId = mockSeriesPolicyAlt.computeId))

  val assetGroupSeriesAlt: Value = assetGroupSeries.copy(
    assetGroupSeries.getAsset.copy(
      groupId = mockGroupPolicyAlt.computeId.some,
      seriesId = mockSeriesPolicyAlt.computeId.some
    )
  )

  val assetGroupAlt: Value = assetGroup.copy(
    assetGroup.getAsset.copy(
      groupId = mockGroupPolicyAlt.computeId.some,
      seriesId = mockSeriesPolicyAlt.computeId.some
    )
  )

  val assetSeriesAlt: Value = assetSeries.copy(
    assetSeries.getAsset.copy(
      groupId = mockGroupPolicyAlt.computeId.some,
      seriesId = mockSeriesPolicyAlt.computeId.some
    )
  )

  val assetGroupSeriesAccumulatorAlt: Value = assetGroupSeriesAccumulator.copy(
    assetGroupSeriesAccumulator.getAsset.copy(
      groupId = mockGroupPolicyAlt.computeId.some,
      seriesId = mockSeriesPolicyAlt.computeId.some
    )
  )

  val assetGroupAccumulatorAlt: Value = assetGroupAccumulator.copy(
    assetGroupAccumulator.getAsset.copy(
      groupId = mockGroupPolicyAlt.computeId.some,
      seriesId = mockSeriesPolicyAlt.computeId.some
    )
  )

  val assetSeriesAccumulatorAlt: Value = assetSeriesAccumulator.copy(
    assetSeriesAccumulator.getAsset.copy(
      groupId = mockGroupPolicyAlt.computeId.some,
      seriesId = mockSeriesPolicyAlt.computeId.some
    )
  )

  val mockValues: Seq[Value] = Seq(
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
    assetGroup.copy(),
    assetGroupAlt, // diff group and series
    assetSeries,
    assetSeries.copy(),
    assetSeriesAlt, // diff group and series
    assetGroupSeriesAccumulator,
    assetGroupSeriesAccumulator.copy(),
    assetGroupSeriesAccumulatorAlt, // diff group and series
    assetGroupAccumulator,
    assetGroupAccumulator.copy(),
    assetGroupAccumulatorAlt, // diff group and series
    assetSeriesAccumulator,
    assetSeriesAccumulator.copy(),
    assetSeriesAccumulatorAlt // diff group and series
  )

  val mockTxos: Seq[Txo] = mockValues.map(valToTxo(_))
}
