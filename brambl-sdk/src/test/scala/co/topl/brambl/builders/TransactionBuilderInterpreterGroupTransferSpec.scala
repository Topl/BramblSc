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
  GroupType
}

class TransactionBuilderInterpreterGroupTransferSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildGroupTransferTransaction > underlying error fails (unsupported token type)") {
    val testTx = txBuilder.buildGroupTransferTransaction(
      GroupType(mockGroupPolicy.computeId),
      mockTxos :+ valToTxo(Value.defaultInstance.withTopl(Value.TOPL(quantity))),
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(testTx, Left(UnableToBuildTransaction(Seq(UserInputError(s"Invalid value type")))))
  }

  test("buildGroupTransferTransaction > quantity to transfer is non positive") {
    val testTx = txBuilder.buildGroupTransferTransaction(
      GroupType(mockGroupPolicy.computeId),
      mockTxos,
      inPredicateLockFull,
      0,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(testTx, Left(UnableToBuildTransaction(Seq(UserInputError(s"quantity to transfer must be positive")))))
  }

  test("buildGroupTransferTransaction > a txo isnt tied to lockPredicateFrom") {
    val testTx = txBuilder.buildGroupTransferTransaction(
      GroupType(mockGroupPolicy.computeId),
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

  test("buildGroupTransferTransaction > non sufficient funds") {
    val testTx = txBuilder.buildGroupTransferTransaction(
      GroupType(mockGroupPolicy.computeId),
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

  test("buildGroupTransferTransaction > fee not satisfied") {
    val testTx = txBuilder.buildGroupTransferTransaction(
      GroupType(mockGroupPolicy.computeId),
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

  test("buildGroupTransferTransaction > [complex] duplicate inputs are merged and split correctly") {
    val testTx = txBuilder.buildGroupTransferTransaction(
      GroupType(mockGroupPolicy.computeId),
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
          UnspentTransactionOutput(inLockFullAddress, groupValue), // recipient
          UnspentTransactionOutput(trivialLockAddress, groupValue),
          UnspentTransactionOutput(trivialLockAddress, value),
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
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroupSeriesAccumulator.copy(assetGroupSeriesAccumulator.value.setQuantity(quantity * 2))
          ),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroupSeriesAccumulator.copy(
              assetGroupSeriesAccumulator.getAsset
                .copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          )
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildGroupTransferTransaction > [simplest case] no change, only 1 output") {
    val txos = Seq(valToTxo(groupValue))
    val testTx = txBuilder.buildGroupTransferTransaction(
      GroupType(mockGroupPolicy.computeId),
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
      .withOutputs(List(UnspentTransactionOutput(inLockFullAddress, groupValue)))
    assertEquals(testTx.toOption.get.computeId, expectedTx.computeId)
  }

  test("buildGroupTransferTransaction > IMMUTABLE asset quantity descriptor in TXOs") {
    val testTx = txBuilder.buildGroupTransferTransaction(
      GroupType(mockGroupPolicy.computeId),
      mockTxos :+ valToTxo(assetGroupSeriesImmutable),
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(
            UserInputError(
              s"All asset tokens must have valid QuantityDescriptorType. We currently only support LIQUID and ACCUMULATOR"
            )
          )
        )
      )
    )
  }

  test("buildGroupTransferTransaction > FRACTIONABLE asset quantity descriptor in TXOs") {
    val testTx = txBuilder.buildGroupTransferTransaction(
      GroupType(mockGroupPolicy.computeId),
      mockTxos :+ valToTxo(assetGroupSeriesFractionable),
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(
            UserInputError(
              s"All asset tokens must have valid QuantityDescriptorType. We currently only support LIQUID and ACCUMULATOR"
            )
          )
        )
      )
    )
  }
}
