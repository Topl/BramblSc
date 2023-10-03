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
      inLockFullAddress,
      trivialLockAddress,
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
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(testTx, Left(UserInputErrors(Seq(UserInputError(s"quantity to transfer must be positive")))))
  }

  test("buildTransferAmountTransaction > a txo isnt tied to lockPredicateFrom") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      LvlType,
      mockTxos :+ valToTxo(value, trivialLockAddress),
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
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
      inLockFullAddress,
      trivialLockAddress,
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
      inLockFullAddress,
      trivialLockAddress,
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
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, value), // recipient
          UnspentTransactionOutput(trivialLockAddress, groupValue.copy(groupValue.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            groupValue.copy(groupValue.getGroup.withGroupId(mockGroupPolicyAlt.computeId))
          ),
          UnspentTransactionOutput(trivialLockAddress, seriesValue.copy(seriesValue.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            seriesValue.copy(seriesValue.getSeries.withSeriesId(mockSeriesPolicyAlt.computeId))
          ),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2))
          ),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroupSeries.copy(
              assetGroupSeries.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          ),
          UnspentTransactionOutput(trivialLockAddress, assetGroup.copy(assetGroup.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroup.copy(
              assetGroup.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          ),
          UnspentTransactionOutput(trivialLockAddress, assetSeries.copy(assetSeries.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetSeries.copy(
              assetSeries.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          ),
          UnspentTransactionOutput(trivialLockAddress, assetGroupSeriesAccumulator),
          UnspentTransactionOutput(trivialLockAddress, assetGroupSeriesAccumulator.copy()),
          UnspentTransactionOutput(trivialLockAddress, assetGroupSeriesAccumulatorAlt),
          UnspentTransactionOutput(trivialLockAddress, assetGroupAccumulator),
          UnspentTransactionOutput(trivialLockAddress, assetGroupAccumulator.copy()),
          UnspentTransactionOutput(trivialLockAddress, assetGroupAccumulatorAlt),
          UnspentTransactionOutput(trivialLockAddress, assetSeriesAccumulator),
          UnspentTransactionOutput(trivialLockAddress, assetSeriesAccumulator.copy()),
          UnspentTransactionOutput(trivialLockAddress, assetSeriesAccumulatorAlt)
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAmountTransaction > [simplest case] no change, only 1 output") {
    val txos = Seq(valToTxo(value))
    val testTx = txBuilder.buildTransferAmountTransaction(
      LvlType,
      txos,
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(txos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(List(UnspentTransactionOutput(inLockFullAddress, value)))
    assertEquals(testTx.toOption.get.computeId, expectedTx.computeId)
  }
}
