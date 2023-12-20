package co.topl.brambl.builders

import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.{
  bigIntAsInt128,
  int128AsBigInt,
  ioTransactionAsTransactionSyntaxOps,
  valueToQuantitySyntaxOps,
  valueToTypeIdentifierSyntaxOps,
  LvlType,
  UnknownType
}

class TransactionBuilderInterpreterTransferAllSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildTransferAllTransaction > unsupported token type (tokenIdentifier)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(UnknownType)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(
              s"When tokenIdentifier is provided, there must be some Txos that match the tokenIdentifier."
            ),
            UserInputError(s"UnknownType tokens are not supported.")
          )
        )
      )
    )
  }

  test("unsupported token type in txos is filtered out/ignored") {
    val testTx = buildTransferAmountTransaction
      .withTxos(mockTxos :+ valToTxo(Value.defaultInstance)) // Value.empty
      .run
    val expectedTx = buildTransferAmountTransaction
      .withTxos(mockTxos) // The only difference is the unsupported txo is not present
      .run
    assert(
      (testTx.isRight && expectedTx.isRight) &&
      sortedTx(testTx.toOption.get).computeId == sortedTx(expectedTx.toOption.get).computeId
    )
  }

  test("buildTransferAllTransaction > All locks don't match") {
    val testTx = buildTransferAllTransaction
      .withTxos(mockTxos :+ valToTxo(lvlValue, trivialLockAddress))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"every lock in the txos must correspond to lockPredicateFrom")
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
        // No more LVLs for recipient. All other tokens go to recipient
        buildRecipientUtxos(mockChange.filterNot(_.value.typeIdentifier == LvlType))
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
        buildChangeUtxos(mockChange.filterNot(_.value.typeIdentifier == LvlType))
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
        // To recipient
        buildRecipientUtxos(List(groupValue.copy(groupValue.value.setQuantity(quantity * 2))))
        ++
        // No more LVLs for change
        buildChangeUtxos(
          mockChange.filterNot(v => List(LvlType, groupValue.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // to recipient
        buildRecipientUtxos(List(groupValue.copy(groupValue.value.setQuantity(quantity * 2))))
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange.filterNot(v => List(LvlType, groupValue.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // to recipient
        buildRecipientUtxos(List(seriesValue.copy(seriesValue.value.setQuantity(quantity * 2))))
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange.filterNot(v => List(LvlType, seriesValue.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // to recipient
        buildRecipientUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(mockChange.filterNot(_.value.typeIdentifier == LvlType))
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
        // to recipient
        buildRecipientUtxos(List(assetGroup.copy(assetGroup.value.setQuantity(quantity * 2))))
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange.filterNot(v => List(LvlType, assetGroup.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // to recipient
        buildRecipientUtxos(List(assetSeries.copy(assetSeries.value.setQuantity(quantity * 2))))
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange.filterNot(v => List(LvlType, assetSeries.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // to recipient
        buildRecipientUtxos(List(assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2))))
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange
            .filterNot(v => List(LvlType, assetGroupSeries.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // to recipient
        buildRecipientUtxos(
          List(
            assetGroupSeriesAccumulator,
            assetGroupSeriesAccumulator.copy() // Have not been aggregated together
          )
        )
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange.filterNot(v =>
            List(LvlType, assetGroupSeriesAccumulator.value.typeIdentifier).contains(v.value.typeIdentifier)
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
        // to recipient
        buildRecipientUtxos(
          List(
            assetGroupAccumulator,
            assetGroupAccumulator.copy() // Have not been aggregated together
          )
        )
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange
            .filterNot(v => List(LvlType, assetGroupAccumulator.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // to recipient
        buildRecipientUtxos(
          List(
            assetSeriesAccumulator,
            assetSeriesAccumulator.copy() // Have not been aggregated together
          )
        )
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange
            .filterNot(v => List(LvlType, assetSeriesAccumulator.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // to recipient
        buildRecipientUtxos(
          List(
            assetGroupSeriesFractionable,
            assetGroupSeriesFractionable.copy() // Have not been aggregated together
          )
        )
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange.filterNot(v =>
            List(LvlType, assetGroupSeriesFractionable.value.typeIdentifier).contains(v.value.typeIdentifier)
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
        // to recipient
        buildRecipientUtxos(
          List(
            assetGroupFractionable,
            assetGroupFractionable.copy() // Have not been aggregated together
          )
        )
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange
            .filterNot(v => List(LvlType, assetGroupFractionable.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // to recipient
        buildRecipientUtxos(
          List(
            assetSeriesFractionable,
            assetSeriesFractionable.copy() // Have not been aggregated together
          )
        )
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange.filterNot(v =>
            List(LvlType, assetSeriesFractionable.value.typeIdentifier).contains(v.value.typeIdentifier)
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
        // to recipient
        buildRecipientUtxos(
          List(
            assetGroupSeriesImmutable,
            assetGroupSeriesImmutable.copy() // Have not been aggregated together
          )
        )
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange.filterNot(v =>
            List(LvlType, assetGroupSeriesImmutable.value.typeIdentifier).contains(v.value.typeIdentifier)
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
        // to recipient
        buildRecipientUtxos(
          List(
            assetGroupImmutable,
            assetGroupImmutable.copy() // Have not been aggregated together
          )
        )
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange
            .filterNot(v => List(LvlType, assetGroupImmutable.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // to recipient
        buildRecipientUtxos(
          List(
            assetSeriesImmutable,
            assetSeriesImmutable.copy() // Have not been aggregated together
          )
        )
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange
            .filterNot(v => List(LvlType, assetSeriesImmutable.value.typeIdentifier).contains(v.value.typeIdentifier))
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer all TOPLs (with no staking reg)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(toplValue.value.typeIdentifier)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        // to recipient
        buildRecipientUtxos(List(toplValue.copy(toplValue.value.setQuantity(BigInt(2)))))
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange
            .filterNot(v => List(LvlType, toplValue.value.typeIdentifier).contains(v.value.typeIdentifier))
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAllTransaction > Transfer all TOPLs (staking reg specified)") {
    val testTx = buildTransferAllTransaction
      .withTokenIdentifier(toplReg1.value.typeIdentifier)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        // to recipient
        buildRecipientUtxos(List(toplReg1))
        ++
        // change due to excess fee
        buildChangeUtxos(List(lvlValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange
            .filterNot(v => List(LvlType, toplReg1.value.typeIdentifier).contains(v.value.typeIdentifier))
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
        // all (except fee) go to recipient
        buildRecipientUtxos(mockChange.filterNot(_.value.typeIdentifier == LvlType))
        ++
        // lvl in excess of fee goes to recipient
        buildRecipientUtxos(List(lvlValue))
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }
}
