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
    Txo(UnspentTransactionOutput(lockAddr, value), UNSPENT, dummyTxoAddress)

  def sortedTx(tx: IoTransaction): IoTransaction =
    tx.withOutputs(tx.outputs.sortBy(_.value.immutable.value.toByteArray.mkString))

  val mockSeriesPolicyAlt: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, dummyTxoAddress.copy(index = 44))
  val mockGroupPolicyAlt: GroupPolicy = GroupPolicy("Mock Group Policy", dummyTxoAddress.copy(index = 55))

  val mockTxos: Seq[Txo] = Seq(
    value,
    value.copy(), // exact duplicate
    groupValue,
    groupValue.copy(), // exact duplicate
    groupValue.copy(groupValue.getGroup.copy(groupId = mockGroupPolicyAlt.computeId)), // diff group
    seriesValue,
    seriesValue.copy(), // exact duplicate
    seriesValue.copy(seriesValue.getSeries.copy(seriesId = mockSeriesPolicyAlt.computeId)), // diff series
    assetGroupSeries,
    assetGroupSeries.copy(), // exact duplicate
    assetGroupSeries.copy(
      assetGroupSeries.getAsset.copy(
        groupId = mockGroupPolicyAlt.computeId.some,
        seriesId = mockSeriesPolicyAlt.computeId.some
      )
    ) // diff group and series
  ).map(valToTxo(_))
}
