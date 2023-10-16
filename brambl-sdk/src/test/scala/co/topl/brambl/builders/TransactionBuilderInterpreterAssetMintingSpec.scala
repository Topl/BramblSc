package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances.lockImmutable
import co.topl.brambl.models.{LockAddress, LockId}
import co.topl.brambl.models.box.FungibilityType.{GROUP, SERIES}
import co.topl.brambl.models.box.QuantityDescriptorType.{ACCUMULATOR, FRACTIONABLE, IMMUTABLE}
import co.topl.brambl.models.box.AssetMintingStatement
import co.topl.brambl.models.transaction.{IoTransaction, UnspentTransactionOutput}
import co.topl.brambl.syntax.{
  assetAsBoxVal,
  bigIntAsInt128,
  groupAsBoxVal,
  ioTransactionAsTransactionSyntaxOps,
  seriesAsBoxVal,
  seriesPolicyAsSeriesPolicySyntaxOps,
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

//  test("token supply unlimited (full series in output)") {
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(inLockFullAddress, seriesValue),
//          UnspentTransactionOutput(trivialLockAddress, assetGroupSeries),
//          UnspentTransactionOutput(inLockFullAddress, groupValue)
//        )
//      )
//    val txRes = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
//  }
//
//  test("mint token supply quantity (series in output)") {
//    val outQuantity: Int128 = BigInt(9)
//    val fullQuantity: Int128 = BigInt(10)
//    val mintedQuantity: Int128 = BigInt(5)
//
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantity = fullQuantity, tokenSupply = 5.some))
//    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, mintedQuantity)
//    val mintedValue = assetGroupSeries.copy(assetGroupSeries.getAsset.copy(quantity = mintedQuantity))
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(inLockFullAddress, seriesVal.copy(seriesVal.getSeries.copy(quantity = outQuantity))),
//          UnspentTransactionOutput(trivialLockAddress, mintedValue),
//          UnspentTransactionOutput(inLockFullAddress, groupValue)
//        )
//      )
//    val txRes = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
//  }
//
//  test("mint multiple token supply quantity (series in output)") {
//    val outQuantity: Int128 = BigInt(5)
//    val fullQuantity: Int128 = BigInt(10)
//    val mintedQuantity: Int128 = BigInt(25)
//
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantity = fullQuantity, tokenSupply = 5.some))
//    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, mintedQuantity)
//    val mintedValue = assetGroupSeries.copy(assetGroupSeries.getAsset.copy(quantity = mintedQuantity))
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(inLockFullAddress, seriesVal.copy(seriesVal.getSeries.copy(quantity = outQuantity))),
//          UnspentTransactionOutput(trivialLockAddress, mintedValue),
//          UnspentTransactionOutput(inLockFullAddress, groupValue)
//        )
//      )
//    val txRes = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
//  }
//
//  test("mint ALL token supply quantity (series not in output)") {
//    val fullQuantity: Int128 = BigInt(10)
//    val mintedQuantity: Int128 = BigInt(50)
//
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantity = fullQuantity, tokenSupply = 5.some))
//    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, mintedQuantity)
//    val mintedValue = assetGroupSeries.copy(assetGroupSeries.getAsset.copy(quantity = mintedQuantity))
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(trivialLockAddress, mintedValue),
//          UnspentTransactionOutput(inLockFullAddress, groupValue)
//        )
//      )
//    val txRes = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
//  }
//
//  test("group and series fungible") {
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
//
//    val testTx = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(inLockFullAddress, seriesValue),
//          UnspentTransactionOutput(trivialLockAddress, assetGroupSeries),
//          UnspentTransactionOutput(inLockFullAddress, groupValue)
//        )
//      )
//    assert(testTx.isRight && testTx.toOption.get.computeId == expectedTx.computeId)
//  }
//
//  test("group fungible") {
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(fungibility = GROUP))
//    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
//
//    val testTx = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(inLockFullAddress, seriesVal),
//          UnspentTransactionOutput(trivialLockAddress, assetGroup),
//          UnspentTransactionOutput(inLockFullAddress, groupValue)
//        )
//      )
//    assert(testTx.isRight && testTx.toOption.get.computeId == expectedTx.computeId)
//  }
//
//  test("series fungible") {
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(fungibility = SERIES))
//    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
//
//    val testTx = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(inLockFullAddress, seriesVal),
//          UnspentTransactionOutput(trivialLockAddress, assetSeries),
//          UnspentTransactionOutput(inLockFullAddress, groupValue)
//        )
//      )
//    assert(testTx.isRight && testTx.toOption.get.computeId == expectedTx.computeId)
//  }
//
//  test("fixedSeries provided") {
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupVal = groupValue.copy(groupValue.getGroup.copy(fixedSeries = mockSeriesPolicy.computeId.some))
//    val groupTxo = valToTxo(groupVal).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
//
//    val testTx = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(inLockFullAddress, seriesValue),
//          UnspentTransactionOutput(trivialLockAddress, assetGroupSeries),
//          UnspentTransactionOutput(inLockFullAddress, groupVal)
//        )
//      )
//    assert(testTx.isRight && testTx.toOption.get.computeId == expectedTx.computeId)
//  }
//
//  test("IMMUTABLE quantity descriptor type") {
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantityDescriptor = IMMUTABLE))
//    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(inLockFullAddress, seriesVal),
//          UnspentTransactionOutput(trivialLockAddress, assetGroupSeriesImmutable),
//          UnspentTransactionOutput(inLockFullAddress, groupValue)
//        )
//      )
//    val txRes = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
//  }
//
//  test("FRACTIONABLE quantity descriptor type") {
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantityDescriptor = FRACTIONABLE))
//    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(inLockFullAddress, seriesVal),
//          UnspentTransactionOutput(trivialLockAddress, assetGroupSeriesFractionable),
//          UnspentTransactionOutput(inLockFullAddress, groupValue)
//        )
//      )
//    val txRes = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
//  }
//
//  test("ACCUMULATOR quantity descriptor type") {
//    val seriesAddr = dummyTxoAddress.copy(index = 1)
//    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantityDescriptor = ACCUMULATOR))
//    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)
//
//    val groupAddr = dummyTxoAddress.copy(index = 2)
//    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)
//
//    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
//
//    val expectedTx = IoTransaction.defaultInstance
//      .withDatum(txDatum)
//      .withMintingStatements(Seq(statement))
//      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
//      .withOutputs(
//        List(
//          UnspentTransactionOutput(inLockFullAddress, seriesVal),
//          UnspentTransactionOutput(trivialLockAddress, assetGroupSeriesAccumulator),
//          UnspentTransactionOutput(inLockFullAddress, groupValue)
//        )
//      )
//    val txRes = txBuilder
//      .buildSimpleAssetMintingTransaction(
//        statement,
//        groupTxo,
//        seriesTxo,
//        inPredicateLockFull,
//        inPredicateLockFull,
//        trivialLockAddress,
//        None,
//        None
//      )
//    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
//  }
}
