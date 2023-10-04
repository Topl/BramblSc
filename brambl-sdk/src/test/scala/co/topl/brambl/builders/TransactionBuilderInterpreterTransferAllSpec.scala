package co.topl.brambl.builders

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
    val testTx = buildTransferAllTransaction
      .withTxos(mockTxos :+ valToTxo(lvlValue, trivialLockAddress))
      .run
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
    val testTx = buildTransferAllTransaction
      .withTxos(Seq.empty)
      .withFee(0)
      .run
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
    val testTx = buildTransferAllTransaction
      .withTxos(Seq.empty)
      .withTokenIdentifier(LvlType)
      .withFee(0)
      .run
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
    val testTx = buildTransferAllTransaction
      .withTxos(Seq(valToTxo(lvlValue)))
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .run
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
    val testTx = buildTransferAllTransaction
      .withFee(3)
      .run
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
    val testTx = buildTransferAllTransaction
      .withFee(2)
      .run
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

  test("buildTransferAllTransaction > exactly amount of LVLs to satisfy fee, tokenIdentifier is LVLs") {
    val testTx = buildTransferAllTransaction
      .withFee(2)
      .withTokenIdentifier(LvlType)
      .run
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

  test("buildTransferAllTransaction > exactly amount of LVLs to satisfy fee, tokenIdentifier is not LVLs") {
    val testTx = buildTransferAllTransaction
      .withFee(2)
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .run
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

  test("buildTransferAllTransaction > Transfer all Group") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .run
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

  test("buildTransferAllTransaction > Transfer all Series") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(seriesValue.value.typeIdentifier)
      .run
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

  test("buildTransferAllTransaction > Transfer all LVLs") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(LvlType)
      .run
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

  test("buildTransferAllTransaction > Transfer all Assets (liquid, group fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetGroup.value.typeIdentifier)
      .run
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

  test("buildTransferAllTransaction > Transfer all Assets (liquid, series fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetSeries.value.typeIdentifier)
      .run
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

  test("buildTransferAllTransaction > Transfer all Assets (liquid, group and series fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetGroupSeries.value.typeIdentifier)
      .run
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

  test("buildTransferAllTransaction > Transfer all Assets (accumulator, group and series fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetGroupSeriesAccumulator.value.typeIdentifier)
      .run
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

  test("buildTransferAllTransaction > Transfer all Assets (accumulator, group fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetGroupAccumulator.value.typeIdentifier)
      .run
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

  test("buildTransferAllTransaction > Transfer all Assets (accumulator, series fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetSeriesAccumulator.value.typeIdentifier)
      .run
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

  test("buildTransferAllTransaction > Transfer all Assets (fractionable, group and series fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetGroupSeriesFractionable.value.typeIdentifier)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
          List(
            assetGroupSeriesFractionable,
            assetGroupSeriesFractionable.copy() // Have not been aggregated together
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
            assetSeriesAccumulator,
            assetSeriesAccumulator.copy(),
            assetSeriesAccumulatorAlt,
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

  test("buildTransferAllTransaction > Transfer all Assets (fractionable, group fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetGroupFractionable.value.typeIdentifier)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
          List(
            assetGroupFractionable,
            assetGroupFractionable.copy() // Have not been aggregated together
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
            assetSeriesAccumulator,
            assetSeriesAccumulator.copy(),
            assetSeriesAccumulatorAlt,
            assetGroupSeriesFractionable,
            assetGroupSeriesFractionable.copy(),
            assetGroupSeriesFractionableAlt,
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

  test("buildTransferAllTransaction > Transfer all Assets (fractionable, series fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetSeriesFractionable.value.typeIdentifier)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
          List(
            assetSeriesFractionable,
            assetSeriesFractionable.copy() // Have not been aggregated together
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
            assetSeriesAccumulator,
            assetSeriesAccumulator.copy(),
            assetSeriesAccumulatorAlt,
            assetGroupSeriesFractionable,
            assetGroupSeriesFractionable.copy(),
            assetGroupSeriesFractionableAlt,
            assetGroupFractionable,
            assetGroupFractionable.copy(),
            assetGroupFractionableAlt,
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

  test("buildTransferAllTransaction > Transfer all Assets (immutable, group and series fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetGroupSeriesImmutable.value.typeIdentifier)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
          List(
            assetGroupSeriesImmutable,
            assetGroupSeriesImmutable.copy() // Have not been aggregated together
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

  test("buildTransferAllTransaction > Transfer all Assets (immutable, group fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetGroupImmutable.value.typeIdentifier)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
          List(
            assetGroupImmutable,
            assetGroupImmutable.copy() // Have not been aggregated together
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

  test("buildTransferAllTransaction > Transfer all Assets (immutable, series fungible)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(assetSeriesImmutable.value.typeIdentifier)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        buildRecipientUtxos(
          List(
            assetSeriesImmutable,
            assetSeriesImmutable.copy() // Have not been aggregated together
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
            assetSeriesImmutableAlt
          )
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer ALL tokens") {
    val testTx = buildTransferAllTransaction.run
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
}
