package co.topl.brambl.builders

import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.{
  bigIntAsInt128,
  int128AsBigInt,
  ioTransactionAsTransactionSyntaxOps,
  valueToQuantitySyntaxOps,
  valueToTypeIdentifierSyntaxOps
}

class TransactionBuilderInterpreterGroupTransferSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildTransferAmountTransaction > underlying error fails (unsupported token type)") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      groupValue.value.typeIdentifier,
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
      groupValue.value.typeIdentifier,
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
      groupValue.value.typeIdentifier,
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
      groupValue.value.typeIdentifier,
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
          Seq(UserInputError(s"All tokens selected to transfer do not have enough funds to transfer"))
        )
      )
    )
  }

  test("buildTransferAmountTransaction > fee not satisfied") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      groupValue.value.typeIdentifier,
      mockTxos,
      inPredicateLockFull,
      1,
      RecipientAddr,
      ChangeAddr,
      3
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
      groupValue.value.typeIdentifier,
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
        buildRecipientUtxos(List(groupValue))
        ++
        buildChangeUtxos(
          List(
            lvlValue,
            groupValue,
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
            assetSeriesAccumulatorAlt,
            assetGroupSeriesFractionable,
            assetGroupSeriesFractionable.copy(),
            assetGroupSeriesFractionableAlt,
            assetGroupFractionable,
            assetGroupFractionable.copy(),
            assetGroupFractionableAlt,
            assetSeriesFractionable,
            assetSeriesFractionable.copy(),
            assetSeriesFractionableAlt,
            assetGroupSeriesImmutable,
            assetGroupSeriesImmutable.copy(),
            assetGroupSeriesImmutableAlt,
            assetGroupImmutable,
            assetGroupImmutable.copy(),
            assetGroupImmutableAlt,
            assetSeriesImmutable,
            assetSeriesImmutable.copy(),
            assetSeriesImmutableAlt
          )
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAmountTransaction > [simplest case] no change, only 1 output") {
    val txos = Seq(valToTxo(groupValue))
    val testTx = txBuilder.buildTransferAmountTransaction(
      groupValue.value.typeIdentifier,
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
      .withOutputs(buildRecipientUtxos(List(groupValue)))
    assertEquals(testTx.toOption.get.computeId, expectedTx.computeId)
  }
}
