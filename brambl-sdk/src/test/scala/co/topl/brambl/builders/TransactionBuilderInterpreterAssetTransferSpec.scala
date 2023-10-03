package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
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
  valueToTypeIdentifierSyntaxOps,
  GroupAndSeriesFungible,
  GroupFungible,
  SeriesFungible
}

class TransactionBuilderInterpreterAssetTransferSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildTransferAmountTransaction > underlying error fails (unsupported token type)") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      assetGroupSeries.value.typeIdentifier,
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
      assetGroupSeries.value.typeIdentifier,
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
      assetGroupSeries.value.typeIdentifier,
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
      assetGroupSeries.value.typeIdentifier,
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
      assetGroupSeries.value.typeIdentifier,
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

  test("buildTransferAmountTransaction > [complex, GROUP_AND_SERIES] duplicate inputs are merged and split correctly") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      assetGroupSeries.value.typeIdentifier,
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
        buildRecipientUtxos(List(assetGroupSeries))
        ++
        buildChangeUtxos(
          List(
            lvlValue,
            groupValue.copy(groupValue.value.setQuantity(quantity * 2)),
            groupValueAlt,
            seriesValue.copy(seriesValue.value.setQuantity(quantity * 2)),
            seriesValueAlt,
            assetGroupSeries,
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

  test("buildTransferAmountTransaction > [complex, GROUP] duplicate inputs are merged and split correctly") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      assetGroup.value.typeIdentifier,
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
        buildRecipientUtxos(List(assetGroup))
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
            assetGroup,
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

  test("buildTransferAmountTransaction > [complex, SERIES] duplicate inputs are merged and split correctly") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      assetSeries.value.typeIdentifier,
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
        buildRecipientUtxos(List(assetSeries))
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
            assetSeries,
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
    val txos = Seq(valToTxo(assetGroupSeries))
    val testTx = txBuilder.buildTransferAmountTransaction(
      assetGroupSeries.value.typeIdentifier,
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
      .withOutputs(buildRecipientUtxos(List(assetGroupSeries)))
    assertEquals(testTx.toOption.get.computeId, expectedTx.computeId)
  }

  test("buildTransferAmountTransaction > IMMUTABLE asset quantity descriptor in transfer type".fail) {
    val testTx = txBuilder.buildTransferAmountTransaction(
      assetGroupSeriesImmutable.value.typeIdentifier,
      mockTxos,
      inPredicateLockFull,
      1,
      RecipientAddr,
      ChangeAddr,
      1
    )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"Invalid asset quantity descriptor type. If identifier is an asset, it must be liquid.")
          )
        )
      )
    )
  }

  test("buildTransferAmountTransaction > FRACTIONABLE asset quantity descriptor in transfer type".fail) {
    val testTx = txBuilder.buildTransferAmountTransaction(
      assetGroupSeriesFractionable.value.typeIdentifier,
      mockTxos,
      inPredicateLockFull,
      1,
      RecipientAddr,
      ChangeAddr,
      1
    )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"Invalid asset quantity descriptor type. If identifier is an asset, it must be liquid.")
          )
        )
      )
    )
  }

  test("buildTransferAmountTransaction > ACCUMULATOR asset quantity descriptor in transfer type") {
    val testTx = txBuilder.buildTransferAmountTransaction(
      assetGroupSeriesAccumulator.value.typeIdentifier,
      mockTxos,
      inPredicateLockFull,
      1,
      RecipientAddr,
      ChangeAddr,
      1
    )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"Invalid asset quantity descriptor type. If identifier is an asset, it must be liquid.")
          )
        )
      )
    )
  }
}
