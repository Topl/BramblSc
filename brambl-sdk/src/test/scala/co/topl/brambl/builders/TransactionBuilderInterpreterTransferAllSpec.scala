package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.{
  bigIntAsInt128,
  int128AsBigInt,
  ioTransactionAsTransactionSyntaxOps,
  valueToQuantitySyntaxOps,
  valueToTypeIdentifierSyntaxOps,
  LvlType
}

class TransactionBuilderInterpreterTransferAllSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildTransferAllTransaction > All locks don't match") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos :+ valToTxo(lvlValue, trivialLockAddress),
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
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
      RecipientAddr,
      ChangeAddr,
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
      RecipientAddr,
      ChangeAddr,
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
      Seq(valToTxo(lvlValue)),
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      0,
      groupValue.value.typeIdentifier.some
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
      RecipientAddr,
      ChangeAddr,
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
      RecipientAddr,
      ChangeAddr,
      2
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
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

  test("buildTransferAllTransaction > exactly amount of LVLs to satisfy fee, tokenIdentifier is LVLs") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      2,
      LvlType.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        // No more LVLs for recipient. All other tokens go to change
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

  test("buildTransferAllTransaction > exactly amount of LVLs to satisfy fee, tokenIdentifier is not LVLs") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      2,
      groupValue.value.typeIdentifier.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(List(groupValue.copy(groupValue.value.setQuantity(quantity * 2))))
        ++
        // No more LVLs for change
        buildChangeUtxos(
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

  test("buildTransferAllTransaction > Transfer all Group") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      1,
      groupValue.value.typeIdentifier.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(List(groupValue.copy(groupValue.value.setQuantity(quantity * 2))))
        ++
        buildChangeUtxos(
          List(
            lvlValue,
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

  test("buildTransferAllTransaction > Transfer all Series") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      1,
      seriesValue.value.typeIdentifier.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(List(seriesValue.copy(seriesValue.value.setQuantity(quantity * 2))))
        ++
        buildChangeUtxos(
          List(
            lvlValue,
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

  test("buildTransferAllTransaction > Transfer all LVLs") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      1,
      LvlType.some
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

  test("buildTransferAllTransaction > Transfer all Assets (liquid, group fungible)") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      1,
      assetGroup.value.typeIdentifier.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(List(assetGroup.copy(assetGroup.value.setQuantity(quantity * 2))))
        ++
        buildChangeUtxos(
          List(
            lvlValue,
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

  test("buildTransferAllTransaction > Transfer all Assets (liquid, series fungible)") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      1,
      assetSeries.value.typeIdentifier.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(List(assetSeries.copy(assetSeries.value.setQuantity(quantity * 2))))
        ++
        buildChangeUtxos(
          List(
            lvlValue,
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

  test("buildTransferAllTransaction > Transfer all Assets (liquid, group and series fungible)") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      1,
      assetGroupSeries.value.typeIdentifier.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(List(assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2))))
        ++
        buildChangeUtxos(
          List(
            lvlValue,
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

  test("buildTransferAllTransaction > Transfer all Assets (accumulator, group and series fungible)") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      1,
      assetGroupSeriesAccumulator.value.typeIdentifier.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
          List(
            assetGroupSeriesAccumulator,
            assetGroupSeriesAccumulator.copy() // Have not been aggregated together
          )
        )
        ++
        buildChangeUtxos(
          List(
            lvlValue,
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

  test("buildTransferAllTransaction > Transfer all Assets (accumulator, group fungible)") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      1,
      assetGroupAccumulator.value.typeIdentifier.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
          List(
            assetGroupAccumulator,
            assetGroupAccumulator.copy() // Have not been aggregated together
          )
        )
        ++
        buildChangeUtxos(
          List(
            lvlValue,
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

  test("buildTransferAllTransaction > Transfer all Assets (accumulator, series fungible)") {
    val testTx = txBuilder.buildTransferAllTransaction(
      mockTxos,
      inPredicateLockFull,
      RecipientAddr,
      ChangeAddr,
      1,
      assetSeriesAccumulator.value.typeIdentifier.some
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
          List(
            assetSeriesAccumulator,
            assetSeriesAccumulator.copy() // Have not been aggregated together
          )
        )
        ++
        buildChangeUtxos(
          List(
            lvlValue,
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
            assetSeriesAccumulatorAlt
          )
        )
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
      RecipientAddr,
      ChangeAddr,
      1
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
          List(
            lvlValue,
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
}
