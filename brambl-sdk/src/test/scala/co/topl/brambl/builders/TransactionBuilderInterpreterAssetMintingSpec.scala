package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances.lockImmutable
import co.topl.brambl.models.{LockAddress, LockId}
import co.topl.brambl.models.box.FungibilityType.{GROUP, SERIES}
import co.topl.brambl.models.box.QuantityDescriptorType.{ACCUMULATOR, FRACTIONABLE, IMMUTABLE}
import co.topl.brambl.models.box.{AssetMintingStatement, Value}
import co.topl.brambl.models.transaction.{IoTransaction, UnspentTransactionOutput}
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
  LvlType
}
import quivr.models.Int128

class TransactionBuilderInterpreterAssetMintingSpec extends TransactionBuilderInterpreterSpecBase {

  test("a Txo's lock not in the lock map") {
    val testTx = buildMintAssetTransaction
      .addTxo(valToTxo(lvlValue, trivialLockAddress))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("every lock in the txos must correspond to a lock in the lock map"))
        )
      )
    )
  }

  test("unsupported token type in txos") {
    val testTx = buildMintAssetTransaction
      .addTxo(valToTxo(Value.defaultInstance)) // Value.empty
      .run
    assertEquals(testTx, Left(UserInputErrors(Seq(UserInputError(s"UnknownType tokens are not supported.")))))
  }

  test("a lock from the lock map not in the txos") {
    val testTx = buildMintAssetTransaction
      .addLock(trivialLockAddress -> trivialOutLock.getPredicate)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("every lock in the lock map must correspond to a lock in the txos"))
        )
      )
    )
  }

  test("groupTokenUtxo is missing") {
    val testTx = buildMintAssetTransaction
      .updateAmsGroupUtxo(dummyTxoAddress.copy(index = 100))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("Input TXOs need to contain exactly one txo matching the groupTokenUtxo"))
        )
      )
    )
  }

  test("groupTokenUtxo is not a group value") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val testTx = buildMintAssetTransaction
      .updateAmsGroupUtxo(newAddr)
      .addTxo(valToTxo(lvlValue, txAddr = newAddr))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("groupTokenUtxo does not contain Group Constructor Tokens"))
        )
      )
    )
  }

  test("seriesTokenUtxo is missing") {
    val testTx = buildMintAssetTransaction
      .updateAmsSeriesUtxo(dummyTxoAddress.copy(index = 100))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("Input TXOs need to contain exactly one txo matching the seriesTokenUtxo"))
        )
      )
    )
  }

  test("seriesTokenUtxo is not a series value") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val testTx = buildMintAssetTransaction
      .updateAmsSeriesUtxo(newAddr)
      .addTxo(valToTxo(lvlValue, txAddr = newAddr))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("seriesTokenUtxo does not contain Series Constructor Tokens"))
        )
      )
    )
  }

  test("invalid minting supply; mint quantity is non positive") {
    val testTx = buildMintAssetTransaction
      .updateAmsQuantity(0)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("quantity to mint must be positive"))
        )
      )
    )
  }

  test("invalid minting supply; token supply limited & mint quantity is not a multiple") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val newSeries = seriesValue.copy(seriesValue.getSeries.copy(tokenSupply = 5.some))
    val testTx = buildMintAssetTransaction
      .updateAmsQuantity(1)
      .updateAmsSeriesUtxo(newAddr)
      .addTxo(valToTxo(newSeries, txAddr = newAddr))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("quantity to mint must be a multiple of token supply"))
        )
      )
    )
  }

  test("invalid minting supply; token supply limited & mint quantity exceeds supply") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val newSeries = seriesValue.copy(seriesValue.getSeries.copy(quantity = BigInt(2), tokenSupply = 5.some))
    val testTx = buildMintAssetTransaction
      .updateAmsQuantity(20)
      .updateAmsSeriesUtxo(newAddr)
      .addTxo(valToTxo(newSeries, txAddr = newAddr))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("quantity to mint must be less than total token supply available."))
        )
      )
    )
  }

  test("invalid fixedSeries") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val newGroup = groupValue.copy(groupValue.getGroup.copy(fixedSeries = mockSeriesPolicyAlt.computeId.some))
    val testTx = buildMintAssetTransaction
      .updateAmsGroupUtxo(newAddr)
      .addTxo(valToTxo(newGroup, txAddr = newAddr))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("fixedSeries does not match provided Series ID"))
        )
      )
    )
  }

  test("insufficient funds for fee") {
    val testTx = buildMintAssetTransaction
      .withFee(3)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("Not enough LVLs in input to satisfy fee"))
        )
      )
    )
  }

  test("TXOS are encumbered by different Lock Addresses") {
    val seriesLock = LockAddress(1, 1, LockId(inLockFull.sizedEvidence.digest.value))
    val newSeries = valToTxo(seriesValue, seriesLock)
    val groupLock = LockAddress(2, 2, LockId(inLockFull.sizedEvidence.digest.value))
    val newGroup = valToTxo(groupValue, groupLock)
    val lvlLock = LockAddress(3, 3, LockId(inLockFull.sizedEvidence.digest.value))
    val newLvl = valToTxo(lvlValue, lvlLock)
    val txRes = buildMintAssetTransaction
      .addTxo(newSeries)
      .addTxo(newGroup)
      .addTxo(newLvl)
      .addLock(seriesLock -> inLockFull.getPredicate)
      .addLock(groupLock -> inLockFull.getPredicate)
      .addLock(lvlLock -> inLockFull.getPredicate)
      .withFee(2)
      .run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo) :+
      newSeries :+ newGroup :+ newLvl
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(List(assetGroupSeries))
        ++
        // fee change
        buildChangeUtxos(List(lvlValue))
        ++
        // non-lvl change (i.e, unaffected by fee)
        buildChangeUtxos(mockChange(allTxos).filterNot(_.value.typeIdentifier == LvlType))
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)

  }

  test("token supply unlimited (full series in output)") {
    val txRes = buildMintAssetTransaction.run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo)
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(List(assetGroupSeries))
        ++
        // fee change
        buildChangeUtxos(List(lvlValue))
        ++
        // non-lvl change (i.e, unaffected by fee)
        buildChangeUtxos(mockChange(allTxos).filterNot(_.value.typeIdentifier == LvlType))
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("mint token supply quantity (series in output)") {
    val outQuantity: Int128 = BigInt(9)
    val fullQuantity: Int128 = BigInt(10)
    val mintedQuantity: Int128 = BigInt(5)
    val newAddr = dummyTxoAddress.copy(index = 100)
    val newPolicyId = mockSeriesPolicy.copy(tokenSupply = 5.some).computeId
    val seriesIn = seriesValue.copy(
      seriesValue.getSeries.copy(seriesId = newPolicyId, quantity = fullQuantity, tokenSupply = 5.some)
    )
    val seriesTxo = valToTxo(seriesIn, txAddr = newAddr)
    val txRes = buildMintAssetTransaction
      .addTxo(seriesTxo)
      .updateAmsSeriesUtxo(newAddr)
      .updateAmsQuantity((mintedQuantity: BigInt).longValue)
      .run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo) :+
      seriesTxo
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(
          List(assetGroupSeries.copy(assetGroupSeries.getAsset.withQuantity(mintedQuantity).withSeriesId(newPolicyId)))
        )
        ++
        // fee change and series change
        buildChangeUtxos(List(lvlValue, seriesIn.copy(seriesIn.getSeries.copy(quantity = outQuantity))))
        ++
        // non-lvl or series change (i.e, unaffected by fee or minting)
        buildChangeUtxos(
          mockChange(allTxos).filterNot(txo =>
            txo.value.typeIdentifier == LvlType || txo.value.typeIdentifier == seriesIn.value.typeIdentifier
          )
        )
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("mint multiple token supply quantity (series in output)") {
    val outQuantity: Int128 = BigInt(5)
    val fullQuantity: Int128 = BigInt(10)
    val mintedQuantity: Int128 = BigInt(25)

    val newAddr = dummyTxoAddress.copy(index = 100)
    val newPolicyId = mockSeriesPolicy.copy(tokenSupply = 5.some).computeId
    val seriesIn = seriesValue.copy(
      seriesValue.getSeries.copy(seriesId = newPolicyId, quantity = fullQuantity, tokenSupply = 5.some)
    )
    val seriesTxo = valToTxo(seriesIn, txAddr = newAddr)
    val txRes = buildMintAssetTransaction
      .addTxo(seriesTxo)
      .updateAmsSeriesUtxo(newAddr)
      .updateAmsQuantity((mintedQuantity: BigInt).longValue)
      .run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo) :+
      seriesTxo
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(
          List(assetGroupSeries.copy(assetGroupSeries.getAsset.withQuantity(mintedQuantity).withSeriesId(newPolicyId)))
        )
        ++
        // fee change and series change
        buildChangeUtxos(List(lvlValue, seriesIn.copy(seriesIn.getSeries.copy(quantity = outQuantity))))
        ++
        // non-lvl or series change (i.e, unaffected by fee or minting)
        buildChangeUtxos(
          mockChange(allTxos).filterNot(txo =>
            txo.value.typeIdentifier == LvlType || txo.value.typeIdentifier == seriesIn.value.typeIdentifier
          )
        )
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("mint ALL token supply quantity (series not in output)") {
    val fullQuantity: Int128 = BigInt(10)
    val mintedQuantity: Int128 = BigInt(50)

    val newAddr = dummyTxoAddress.copy(index = 100)
    val newPolicyId = mockSeriesPolicy.copy(tokenSupply = 5.some).computeId
    val seriesIn = seriesValue.copy(
      seriesValue.getSeries.copy(seriesId = newPolicyId, quantity = fullQuantity, tokenSupply = 5.some)
    )
    val seriesTxo = valToTxo(seriesIn, txAddr = newAddr)
    val txRes = buildMintAssetTransaction
      .addTxo(seriesTxo)
      .updateAmsSeriesUtxo(newAddr)
      .updateAmsQuantity((mintedQuantity: BigInt).longValue)
      .run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo) :+
      seriesTxo
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(
          List(assetGroupSeries.copy(assetGroupSeries.getAsset.withQuantity(mintedQuantity).withSeriesId(newPolicyId)))
        )
        ++
        // fee change. no series change
        buildChangeUtxos(List(lvlValue))
        ++
        // non-lvl or series change (i.e, unaffected by fee or minting)
        buildChangeUtxos(
          mockChange(allTxos).filterNot(txo =>
            txo.value.typeIdentifier == LvlType || txo.value.typeIdentifier == seriesIn.value.typeIdentifier
          )
        )
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("group fungible") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val newPolicyId = mockSeriesPolicy.copy(fungibility = GROUP).computeId
    val seriesIn = seriesValue.copy(seriesValue.getSeries.copy(seriesId = newPolicyId, fungibility = GROUP))
    val seriesTxo = valToTxo(seriesIn, txAddr = newAddr)
    val txRes = buildMintAssetTransaction
      .addTxo(seriesTxo)
      .updateAmsSeriesUtxo(newAddr)
      .run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo) :+
      seriesTxo
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(List(assetGroup.copy(assetGroup.getAsset.withSeriesId(newPolicyId))))
        ++
        // fee change
        buildChangeUtxos(List(lvlValue))
        ++
        // non-lvl (i.e, unaffected by fee)
        buildChangeUtxos(mockChange(allTxos).filterNot(txo => txo.value.typeIdentifier == LvlType))
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("series fungible") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val newPolicyId = mockSeriesPolicy.copy(fungibility = SERIES).computeId
    val seriesIn = seriesValue.copy(seriesValue.getSeries.copy(seriesId = newPolicyId, fungibility = SERIES))
    val seriesTxo = valToTxo(seriesIn, txAddr = newAddr)
    val txRes = buildMintAssetTransaction
      .addTxo(seriesTxo)
      .updateAmsSeriesUtxo(newAddr)
      .run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo) :+
      seriesTxo
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(List(assetSeries.copy(assetSeries.getAsset.withSeriesId(newPolicyId))))
        ++
        // fee change
        buildChangeUtxos(List(lvlValue))
        ++
        // non-lvl (i.e, unaffected by fee)
        buildChangeUtxos(mockChange(allTxos).filterNot(txo => txo.value.typeIdentifier == LvlType))
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("fixedSeries provided") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val newPolicyId = mockGroupPolicy.copy(fixedSeries = seriesValue.getSeries.seriesId.some).computeId
    val groupIn = groupValue.copy(
      groupValue.getGroup.copy(groupId = newPolicyId, fixedSeries = seriesValue.getSeries.seriesId.some)
    )
    val groupTxo = valToTxo(groupIn, txAddr = newAddr)
    val txRes = buildMintAssetTransaction
      .addTxo(groupTxo)
      .updateAmsGroupUtxo(newAddr)
      .run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo) :+
      groupTxo
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(List(assetGroupSeries.copy(assetGroupSeries.getAsset.withGroupId(newPolicyId))))
        ++
        // fee change
        buildChangeUtxos(List(lvlValue))
        ++
        // non-lvl (i.e, unaffected by fee)
        buildChangeUtxos(mockChange(allTxos).filterNot(txo => txo.value.typeIdentifier == LvlType))
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("IMMUTABLE quantity descriptor type") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val newPolicyId = mockSeriesPolicy.copy(quantityDescriptor = IMMUTABLE).computeId
    val seriesIn = seriesValue.copy(seriesValue.getSeries.copy(seriesId = newPolicyId, quantityDescriptor = IMMUTABLE))
    val seriesTxo = valToTxo(seriesIn, txAddr = newAddr)
    val txRes = buildMintAssetTransaction
      .addTxo(seriesTxo)
      .updateAmsSeriesUtxo(newAddr)
      .run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo) :+
      seriesTxo
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(
          List(assetGroupSeriesImmutable.copy(assetGroupSeriesImmutable.getAsset.withSeriesId(newPolicyId)))
        )
        ++
        // fee change
        buildChangeUtxos(List(lvlValue))
        ++
        // non-lvl (i.e, unaffected by fee)
        buildChangeUtxos(mockChange(allTxos).filterNot(txo => txo.value.typeIdentifier == LvlType))
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("FRACTIONABLE quantity descriptor type") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val newPolicyId = mockSeriesPolicy.copy(quantityDescriptor = FRACTIONABLE).computeId
    val seriesIn =
      seriesValue.copy(seriesValue.getSeries.copy(seriesId = newPolicyId, quantityDescriptor = FRACTIONABLE))
    val seriesTxo = valToTxo(seriesIn, txAddr = newAddr)
    val txRes = buildMintAssetTransaction
      .addTxo(seriesTxo)
      .updateAmsSeriesUtxo(newAddr)
      .run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo) :+
      seriesTxo
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(
          List(assetGroupSeriesFractionable.copy(assetGroupSeriesFractionable.getAsset.withSeriesId(newPolicyId)))
        )
        ++
        // fee change
        buildChangeUtxos(List(lvlValue))
        ++
        // non-lvl (i.e, unaffected by fee)
        buildChangeUtxos(mockChange(allTxos).filterNot(txo => txo.value.typeIdentifier == LvlType))
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("ACCUMULATOR quantity descriptor type") {
    val newAddr = dummyTxoAddress.copy(index = 100)
    val newPolicyId = mockSeriesPolicy.copy(quantityDescriptor = ACCUMULATOR).computeId
    val seriesIn =
      seriesValue.copy(seriesValue.getSeries.copy(seriesId = newPolicyId, quantityDescriptor = ACCUMULATOR))
    val seriesTxo = valToTxo(seriesIn, txAddr = newAddr)
    val txRes = buildMintAssetTransaction
      .addTxo(seriesTxo)
      .updateAmsSeriesUtxo(newAddr)
      .run
    val allTxos = mockTxos :+
      valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+
      valToTxo(seriesValue, txAddr = mockSeriesPolicyAlt.registrationUtxo) :+
      seriesTxo
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(mockAssetMintingStatement))
      .withInputs(buildStxos(allTxos))
      .withOutputs(
        // minted output
        buildRecipientUtxos(
          List(assetGroupSeriesAccumulator.copy(assetGroupSeriesAccumulator.getAsset.withSeriesId(newPolicyId)))
        )
        ++
        // fee change
        buildChangeUtxos(List(lvlValue))
        ++
        // non-lvl (i.e, unaffected by fee)
        buildChangeUtxos(mockChange(allTxos).filterNot(txo => txo.value.typeIdentifier == LvlType))
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }
}
