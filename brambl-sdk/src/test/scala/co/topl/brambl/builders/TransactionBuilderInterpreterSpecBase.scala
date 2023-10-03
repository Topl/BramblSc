package co.topl.brambl.builders

import cats.Id
import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.MockHelpers
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances.valueImmutable
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.box.Value.Asset
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

  val RecipientAddr: LockAddress = inLockFullAddress
  val ChangeAddr: LockAddress = trivialLockAddress

  def valToTxo(value: Value, lockAddr: LockAddress = inLockFullAddress): Txo =
    Txo(valToUtxo(value, lockAddr), UNSPENT, dummyTxoAddress)

  def valToUtxo(value: Value, lockAddr: LockAddress = inLockFullAddress): UnspentTransactionOutput =
    UnspentTransactionOutput(lockAddr, value)

  def sortedTx(tx: IoTransaction): IoTransaction = tx
    .withOutputs(tx.outputs.sortBy(_.value.immutable.value.toByteArray.mkString))
    .withInputs(tx.inputs.sortBy(_.value.immutable.value.toByteArray.mkString))

  def toAltAsset(asset: Asset): Asset = asset.copy(
    groupId = mockGroupPolicyAlt.computeId.some,
    seriesId = mockSeriesPolicyAlt.computeId.some
  )

  def buildStxos(txos: Seq[Txo] = mockTxos): Seq[SpentTransactionOutput] =
    txos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value))

  def buildUtxos(values: Seq[Value], lockAddr: LockAddress): Seq[UnspentTransactionOutput] =
    values.map(valToUtxo(_, lockAddr))

  def buildRecipientUtxos(values: Seq[Value]): Seq[UnspentTransactionOutput] = buildUtxos(values, RecipientAddr)
  def buildChangeUtxos(values:    Seq[Value]): Seq[UnspentTransactionOutput] = buildUtxos(values, ChangeAddr)

  val mockSeriesPolicyAlt: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, dummyTxoAddress.copy(index = 44))
  val mockGroupPolicyAlt: GroupPolicy = GroupPolicy("Mock Group Policy", dummyTxoAddress.copy(index = 55))

  val groupValueAlt: Value = groupValue.copy(groupValue.getGroup.copy(groupId = mockGroupPolicyAlt.computeId))

  val seriesValueAlt: Value = seriesValue.copy(seriesValue.getSeries.copy(seriesId = mockSeriesPolicyAlt.computeId))

  val assetGroupSeriesAlt: Value = assetGroupSeries.copy(toAltAsset(assetGroupSeries.getAsset))

  val assetGroupAlt: Value = assetGroup.copy(toAltAsset(assetGroup.getAsset))

  val assetSeriesAlt: Value = assetSeries.copy(toAltAsset(assetSeries.getAsset))

  val assetGroupSeriesAccumulatorAlt: Value =
    assetGroupSeriesAccumulator.copy(toAltAsset(assetGroupSeriesAccumulator.getAsset))

  val assetGroupAccumulatorAlt: Value = assetGroupAccumulator.copy(toAltAsset(assetGroupAccumulator.getAsset))

  val assetSeriesAccumulatorAlt: Value = assetSeriesAccumulator.copy(toAltAsset(assetSeriesAccumulator.getAsset))

  val mockValues: Seq[Value] = Seq(
    lvlValue,
    lvlValue.copy(), // exact duplicate
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
