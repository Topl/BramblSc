package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.models.box.QuantityDescriptorType.{ACCUMULATOR, FRACTIONABLE, IMMUTABLE, LIQUID}
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
  GroupType,
  LvlType,
  SeriesFungible,
  SeriesType
}

class TransactionBuilderInterpreterTransferAllSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildTransferAllTransaction > All locks don't match") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos :+ valToTxo(value, trivialLockAddress),
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"every lock does not correspond to fromLockAddr")
          )
        )
      )
    )
  }

  test("buildTransferAllTransaction > empty TXOs, tokenIdentifier not provided") {
    val testTx = txBuilder.buildTransferAllTransaction(
      Seq.empty,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"There must be at least one Txo to transfer.")
          )
        )
      )
    )
  }

  test("buildTransferAllTransaction > empty TXOs, tokenIdentifier is provided") {
    val testTx = txBuilder.buildTransferAllTransaction(
      Seq.empty,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      0,
      LvlType.some
    )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"When tokenIdentifier is provided, there must be some Txos that match the tokenIdentifier.")
          )
        )
      )
    )
  }

  test("buildTransferAllTransaction > tokenIdentifier is provided but does not exist in TXOs") {
    val testTx = txBuilder.buildTransferAllTransaction(
      Seq(valToTxo(value, inLockFullAddress)),
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      0,
      GroupType(mockGroupPolicy.computeId).some
    )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"When tokenIdentifier is provided, there must be some Txos that match the tokenIdentifier.")
          )
        )
      )
    )
  }

  test("buildTransferAllTransaction > not enough LVLs to satisfy fee") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      3
    )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"Not enough LVLs in input to satisfy fee")
          )
        )
      )
    )
  }

  test("buildTransferAllTransaction > exactly amount of LVLs to satisfy fee, tokenIdentifier not provided") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      2
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        // No more LVLs for recipient. All other tokens go to recipient
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
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, inLockFullAddress)) // recipient
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > exactly amount of LVLs to satisfy fee, tokenIdentifier is LVLs") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      2,
      LvlType.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        // No more LVLs for recipient. All other tokens go to change
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
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, trivialLockAddress)) // change
      )
    println(testTx.swap.toOption.get)
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > exactly amount of LVLs to satisfy fee, tokenIdentifier is not LVLs") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      2,
      GroupType(mockGroupPolicy.computeId).some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(groupValue.copy(groupValue.value.setQuantity(quantity * 2)))
          .map(valToUtxo(_, inLockFullAddress)) // recipient
        ++
        // No more LVLs for change
        List(
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
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, trivialLockAddress)) // change
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer all Group") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      1,
      GroupType(mockGroupPolicy.computeId).some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(groupValue.copy(groupValue.value.setQuantity(quantity * 2)))
          .map(valToUtxo(_, inLockFullAddress)) // recipient
        ++
        List(
          value,
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
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, trivialLockAddress)) // change
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer all Series") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      1,
      SeriesType(mockSeriesPolicy.computeId).some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(seriesValue.copy(seriesValue.value.setQuantity(quantity * 2)))
          .map(valToUtxo(_, inLockFullAddress)) // recipient
        ++
        List(
          value,
          groupValue.copy(groupValue.value.setQuantity(quantity * 2)),
          groupValueAlt,
          seriesValueAlt,
          assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2)),
          assetGroupSeriesAlt,
          assetGroup.copy(assetGroup.value.setQuantity(quantity * 2)),
          assetGroupAlt,
          assetSeries.copy(assetSeries.value.setQuantity(quantity * 2)),
          assetSeriesAlt,
          assetGroupSeriesAccumulator,
          assetGroupSeriesAccumulator.copy(),
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, trivialLockAddress)) // change
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer all LVLs") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      1,
      LvlType.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(value).map(valToUtxo(_, inLockFullAddress)) // recipient
        ++
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
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, trivialLockAddress)) // change
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer all Assets (liquid, group fungible)") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      1,
      GroupFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId.value,
        LIQUID
      ).some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(assetGroup.copy(assetGroup.value.setQuantity(quantity * 2)))
          .map(valToUtxo(_, inLockFullAddress)) // recipient
        ++
        List(
          value,
          groupValue.copy(groupValue.value.setQuantity(quantity * 2)),
          groupValueAlt,
          seriesValue.copy(seriesValue.value.setQuantity(quantity * 2)),
          seriesValueAlt,
          assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2)),
          assetGroupSeriesAlt,
          assetGroupAlt,
          assetSeries.copy(assetSeries.value.setQuantity(quantity * 2)),
          assetSeriesAlt,
          assetGroupSeriesAccumulator,
          assetGroupSeriesAccumulator.copy(),
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, trivialLockAddress)) // change
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer all Assets (liquid, series fungible)") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      1,
      SeriesFungible(
        mockSeriesPolicy.computeId,
        mockGroupPolicy.computeId.value,
        LIQUID
      ).some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(assetSeries.copy(assetSeries.value.setQuantity(quantity * 2)))
          .map(valToUtxo(_, inLockFullAddress)) // recipient
        ++
        List(
          value,
          groupValue.copy(groupValue.value.setQuantity(quantity * 2)),
          groupValueAlt,
          seriesValue.copy(seriesValue.value.setQuantity(quantity * 2)),
          seriesValueAlt,
          assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2)),
          assetGroupSeriesAlt,
          assetGroup.copy(assetGroup.value.setQuantity(quantity * 2)),
          assetGroupAlt,
          assetSeriesAlt,
          assetGroupSeriesAccumulator,
          assetGroupSeriesAccumulator.copy(),
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, trivialLockAddress)) // change
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer all Assets (liquid, group and series fungible)") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      1,
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        LIQUID
      ).some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2)))
          .map(valToUtxo(_, inLockFullAddress)) // recipient
        ++
        List(
          value,
          groupValue.copy(groupValue.value.setQuantity(quantity * 2)),
          groupValueAlt,
          seriesValue.copy(seriesValue.value.setQuantity(quantity * 2)),
          seriesValueAlt,
          assetGroupSeriesAlt,
          assetGroup.copy(assetGroup.value.setQuantity(quantity * 2)),
          assetGroupAlt,
          assetSeries.copy(assetSeries.value.setQuantity(quantity * 2)),
          assetSeriesAlt,
          assetGroupSeriesAccumulator,
          assetGroupSeriesAccumulator.copy(),
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, trivialLockAddress)) // change
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer all Assets (accumulator, group and series fungible)") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      1,
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        ACCUMULATOR
      ).some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(
          assetGroupSeriesAccumulator,
          assetGroupSeriesAccumulator.copy() // Have not been aggregated together
        ).map(valToUtxo(_, inLockFullAddress)) // recipient
        ++
        List(
          value,
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
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, trivialLockAddress)) // change
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer ALL tokens") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(
          value,
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
          assetGroupSeriesAccumulatorAlt
        ).map(valToUtxo(_, trivialLockAddress)) // all to recipient
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }
}
