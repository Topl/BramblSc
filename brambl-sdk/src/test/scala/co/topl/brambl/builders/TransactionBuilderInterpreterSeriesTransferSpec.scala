package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.builders.TransactionBuilderApi.UnableToBuildTransaction
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
  SeriesType
}

class TransactionBuilderInterpreterSeriesTransferSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildSeriesTransferTransaction > underlying error fails (unsupported token type)") {
    val testTx = txBuilder.buildSeriesTransferTransaction(
      SeriesType(mockSeriesPolicy.computeId),
      mockTxos :+ valToTxo(Value.defaultInstance.withTopl(Value.TOPL(quantity))),
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(testTx, Left(UnableToBuildTransaction(Seq(UserInputError(s"Invalid value type")))))
  }

  test("buildSeriesTransferTransaction > quantity to transfer is non positive") {
    val testTx = txBuilder.buildSeriesTransferTransaction(
      SeriesType(mockSeriesPolicy.computeId),
      mockTxos,
      inPredicateLockFull,
      0,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(testTx, Left(UnableToBuildTransaction(Seq(UserInputError(s"quantity to transfer must be positive")))))
  }

  test("buildSeriesTransferTransaction > a txo isnt tied to lockPredicateFrom") {
    val testTx = txBuilder.buildSeriesTransferTransaction(
      SeriesType(mockSeriesPolicy.computeId),
      mockTxos :+ valToTxo(value, trivialLockAddress),
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(
      testTx,
      Left(UnableToBuildTransaction(Seq(UserInputError(s"every lock does not correspond to fromLockAddr"))))
    )
  }

  test("buildSeriesTransferTransaction > a txo is an asset with unsupported fungibility") {
    val testTx = txBuilder.buildSeriesTransferTransaction(
      SeriesType(mockSeriesPolicy.computeId),
      mockTxos :+ valToTxo(assetGroup),
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(
            UserInputError(
              s"All asset tokens must have valid fungibility type. We currently only support GROUP_AND_SERIES"
            )
          )
        )
      )
    )
  }

  test("buildSeriesTransferTransaction > non sufficient funds") {
    val testTx = txBuilder.buildSeriesTransferTransaction(
      SeriesType(mockSeriesPolicy.computeId),
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
        UnableToBuildTransaction(
          Seq(UserInputError(s"All tokens selected to transfer do not have enough funds to transfer"))
        )
      )
    )
  }

  test("buildSeriesTransferTransaction > fee not satisfied") {
    val testTx = txBuilder.buildSeriesTransferTransaction(
      SeriesType(mockSeriesPolicy.computeId),
      mockTxos,
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      3
    )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError(s"Not enough LVLs in input to satisfy fee"))
        )
      )
    )
  }

  test("buildSeriesTransferTransaction > [complex] duplicate inputs are merged and split correctly") {
    val testTx = txBuilder.buildSeriesTransferTransaction(
      SeriesType(mockSeriesPolicy.computeId),
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
          UnspentTransactionOutput(inLockFullAddress, seriesValue), // recipient
          UnspentTransactionOutput(trivialLockAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, value),
          UnspentTransactionOutput(trivialLockAddress, groupValue.copy(groupValue.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            groupValue.copy(groupValue.getGroup.withGroupId(mockGroupPolicyAlt.computeId))
          ),
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
          )
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildSeriesTransferTransaction > [simplest case] no change, only 1 output") {
    val txos = Seq(valToTxo(seriesValue))
    val testTx = txBuilder.buildSeriesTransferTransaction(
      SeriesType(mockSeriesPolicy.computeId),
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
      .withOutputs(List(UnspentTransactionOutput(inLockFullAddress, seriesValue)))
    assertEquals(testTx.toOption.get.computeId, expectedTx.computeId)
  }
}