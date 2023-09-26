package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.builders.TransactionBuilderApi.UnableToBuildTransaction
import co.topl.brambl.models.box.QuantityDescriptorType.{ACCUMULATOR, FRACTIONABLE, IMMUTABLE}
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
  GroupAndSeriesFungible,
  GroupFungible,
  SeriesFungible
}

class TransactionBuilderInterpreterAssetTransferAccumulatorSpec extends TransactionBuilderInterpreterSpecBase {

  private val otherOutputs = List(
    UnspentTransactionOutput(
      trivialLockAddress,
      assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2))
    ),
    UnspentTransactionOutput(trivialLockAddress, value),
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
      assetGroupSeriesAccumulator.copy(
        assetGroupSeriesAccumulator.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
      )
    )
  )

  test("buildAssetTransferTransaction > [complex] Transfer all ACCUMULATOR type (no change)") {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        ACCUMULATOR
      ),
      mockTxos,
      inPredicateLockFull,
      2, // 2 TXOS with 1 quantity each.
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        otherOutputs :+
        // 1 TXO with 2 quantity (they were aggregated together)
        UnspentTransactionOutput(
          inLockFullAddress,
          assetGroupSeriesAccumulator.copy(assetGroupSeriesAccumulator.value.setQuantity(quantity * 2))
        ) // recipient
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  // TODO: Support the failures
  test("buildAssetTransferTransaction > [complex] Transfer partial ACCUMULATOR type with whole change".fail) {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        ACCUMULATOR
      ),
      mockTxos, // Contains 2 transfer-type TXOs with 1 quantity each.
      inPredicateLockFull,
      1, // Transfer 1 TXO, leaving 1 TXO with 1 quantity (whole change).
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        otherOutputs :+
        // 2 TXOs with 1 quantity each (neither were aggregated nor split)
        UnspentTransactionOutput(inLockFullAddress, assetGroupSeriesAccumulator) // recipient
        :+ UnspentTransactionOutput(inLockFullAddress, assetGroupSeriesAccumulator.copy()) // whole change
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildAssetTransferTransaction > [complex] Transfer partial ACCUMULATOR type with whole change".fail) {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        ACCUMULATOR
      ),
      // Contains 4 transfer-type TXOs with 1 quantity each.
      mockTxos :+ valToTxo(assetGroupSeriesAccumulator.copy()) :+ valToTxo(assetGroupSeriesAccumulator.copy()),
      inPredicateLockFull,
      2, // Transfer 2 TXO, leaving 1 TXO with 2 quantity (whole change).
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        otherOutputs :+
        // 2 TXOs with 2 quantity each (both were aggregated but neither was split)
        UnspentTransactionOutput(
          inLockFullAddress,
          assetGroupSeriesAccumulator.copy(assetGroupSeriesAccumulator.value.setQuantity(quantity * 2))
        ) // recipient
        :+ UnspentTransactionOutput(
          inLockFullAddress,
          assetGroupSeriesAccumulator.copy(assetGroupSeriesAccumulator.value.setQuantity(quantity * 2))
        ) // whole change
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildAssetTransferTransaction > [failure] Transfer partial ACCUMULATOR type that requires splitting".fail) {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        ACCUMULATOR
      ),
      // Contains 3 transfer-type TXOs with 1, 1, and 4 quantities respectively.
      mockTxos :+ valToTxo(assetGroupSeriesAccumulator.copy(assetGroupSeriesAccumulator.value.setQuantity(4: BigInt))),
      inPredicateLockFull,
      3, // The only way to achieve this is to split token with quantity 4.
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(
            UserInputError(s"Unable to transfer ACCUMULATOR type without splitting.")
          )
        )
      )
    )
  }
}
