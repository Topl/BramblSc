package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.{
  assetAsBoxVal,
  bigIntAsInt128,
  groupAsBoxVal,
  groupPolicyAsGroupPolicySyntaxOps,
  int128AsBigInt,
  ioTransactionAsTransactionSyntaxOps,
  seriesAsBoxVal,
  seriesPolicyAsSeriesPolicySyntaxOps,
  valueToQuantitySyntaxOps,
  LvlType
}

class TransactionBuilderInterpreterLvlsTransferSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildTransferAmountTransaction > underlying error fails (unsupported token type)") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      LvlType,
      mockTxos :+ valToTxo(Value.defaultInstance.withTopl(Value.TOPL(quantity))),
      inPredicateLockFull,
      1,
      RecipientAddr,
      ChangeAddr,
      0
    )
    assertEquals(testTx, Left(UserInputErrors(Seq(UserInputError(s"Invalid value type")))))
  }

  test("buildTransferAmountTransaction > quantity to transfer is non positive") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      LvlType,
      mockTxos,
      inPredicateLockFull,
      0,
      RecipientAddr,
      ChangeAddr,
      0
    )
    assertEquals(testTx, Left(UserInputErrors(Seq(UserInputError(s"quantity to transfer must be positive")))))
  }

  test("buildTransferAmountTransaction > a txo isnt tied to lockPredicateFrom") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      LvlType,
      mockTxos :+ valToTxo(lvlValue, trivialLockAddress),
      inPredicateLockFull,
      1,
      RecipientAddr,
      ChangeAddr,
      0
    )
    assertEquals(
      testTx,
      Left(UserInputErrors(Seq(UserInputError(s"every lock does not correspond to fromLockAddr"))))
    )
  }

  test("buildTransferAmountTransaction > non sufficient funds") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      LvlType,
      mockTxos,
      inPredicateLockFull,
      4,
      RecipientAddr,
      ChangeAddr,
      0
    )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"All tokens selected to transfer do not have enough funds to transfer"),
            UserInputError(s"Not enough LVLs in input to satisfy fee")
          )
        )
      )
    )
  }

  test("buildTransferAmountTransaction > fee not satisfied") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      LvlType,
      mockTxos,
      inPredicateLockFull,
      1,
      RecipientAddr,
      ChangeAddr,
      2
    )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"Not enough LVLs in input to satisfy fee"))
        )
      )
    )
  }

  test("buildTransferAmountTransaction > [complex] duplicate inputs are merged and split correctly") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      LvlType,
      mockTxos,
      inPredicateLockFull,
      1,
      RecipientAddr,
      ChangeAddr,
      1
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(List(lvlValue))
        ++
        buildChangeUtxos(
          List(
            groupValue.copy(groupValue.value.setQuantity(quantity * 2)),
            groupValueAlt,
            seriesValue.copy(seriesValue.value.setQuantity(quantity * 2)),
            seriesValueAlt,
            assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2)),
            assetGroupSeriesAlt,
            assetGroup.copy(assetGroup.value.setQuantity(quantity * 2)),
            assetGroupAlt,
            assetSeries.copy(assetSeries.value.setQuantity(quantity * 2)),
            assetSeriesAlt,
            assetGroupSeriesAccumulator,
            assetGroupSeriesAccumulator.copy(),
            assetGroupSeriesAccumulatorAlt,
            assetGroupAccumulator,
            assetGroupAccumulator.copy(),
            assetGroupAccumulatorAlt,
            assetSeriesAccumulator,
            assetSeriesAccumulator.copy(),
            assetSeriesAccumulatorAlt
          )
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAmountTransaction > [simplest case] no change, only 1 output") {
    val txos = Seq(valToTxo(lvlValue))
    val testTx = txBuilder.buildTransferAmountTransaction(
      LvlType,
      txos,
      inPredicateLockFull,
      1,
      RecipientAddr,
      ChangeAddr,
      0
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos(txos))
      .withOutputs(buildRecipientUtxos(List(lvlValue)))
    assertEquals(testTx.toOption.get.computeId, expectedTx.computeId)
  }
}
