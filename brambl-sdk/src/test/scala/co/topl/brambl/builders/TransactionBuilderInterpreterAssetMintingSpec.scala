package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.FungibilityType.{GROUP, GROUP_AND_SERIES, SERIES}
import co.topl.brambl.models.box.QuantityDescriptorType.{ACCUMULATOR, FRACTIONABLE, IMMUTABLE}
import co.topl.brambl.models.box.{AssetMintingStatement, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.{
  assetAsBoxVal,
  bigIntAsInt128,
  groupAsBoxVal,
  groupPolicyAsGroupPolicySyntaxOps,
  ioTransactionAsTransactionSyntaxOps,
  seriesAsBoxVal,
  seriesPolicyAsSeriesPolicySyntaxOps
}
import co.topl.genus.services.Txo
import co.topl.genus.services.TxoState.UNSPENT
import com.google.protobuf.ByteString
import quivr.models.Int128

class TransactionBuilderInterpreterAssetMintingSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildSimpleAssetMintingTransaction > success, token supply unlimited (full series in output)") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, assetGroupSeries),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, mint token supply quantity (series in output)") {
    val outQuantity: Int128 = BigInt(9)
    val fullQuantity: Int128 = BigInt(10)
    val mintedQuantity: Int128 = BigInt(5)

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantity = fullQuantity, tokenSupply = 5.some))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, mintedQuantity)
    val mintedValue = assetGroupSeries.copy(assetGroupSeries.getAsset.copy(quantity = mintedQuantity))

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesVal.copy(seriesVal.getSeries.copy(quantity = outQuantity))),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, mint multiple token supply quantity (series in output)") {
    val outQuantity: Int128 = BigInt(5)
    val fullQuantity: Int128 = BigInt(10)
    val mintedQuantity: Int128 = BigInt(25)

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantity = fullQuantity, tokenSupply = 5.some))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, mintedQuantity)
    val mintedValue = assetGroupSeries.copy(assetGroupSeries.getAsset.copy(quantity = mintedQuantity))

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesVal.copy(seriesVal.getSeries.copy(quantity = outQuantity))),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, mint ALL token supply quantity (series not in output)") {
    val fullQuantity: Int128 = BigInt(10)
    val mintedQuantity: Int128 = BigInt(50)

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantity = fullQuantity, tokenSupply = 5.some))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, mintedQuantity)
    val mintedValue = assetGroupSeries.copy(assetGroupSeries.getAsset.copy(quantity = mintedQuantity))

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, group and series fungible") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, assetGroupSeries),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    assert(testTx.isRight && testTx.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, group fungible") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(fungibility = GROUP))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesVal),
          UnspentTransactionOutput(trivialLockAddress, assetGroup),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    assert(testTx.isRight && testTx.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, series fungible") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(fungibility = SERIES))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesVal),
          UnspentTransactionOutput(trivialLockAddress, assetSeries),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    assert(testTx.isRight && testTx.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, fixedSeries provided") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupVal = groupValue.copy(groupValue.getGroup.copy(fixedSeries = mockSeriesPolicy.computeId.some))
    val groupTxo = valToTxo(groupVal).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, assetGroupSeries),
          UnspentTransactionOutput(inLockFullAddress, groupVal)
        )
      )
    assert(testTx.isRight && testTx.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > invalid groupTxo") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"groupTxo does not match groupTokenUtxo"))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > invalid seriesTxo") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = valToTxo(seriesValue)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"seriesTxo does not match seriesTokenUtxo"))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > invalid groupUtxo") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(lvlValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"groupTxo does not contain Group Constructor Tokens"))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > invalid seriesUtxo") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = valToTxo(lvlValue).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"seriesTxo does not contain Series Constructor Tokens"))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > invalid groupLock") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        trivialOutLock.getPredicate,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"groupLock does not correspond to groupTxo"))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > invalid seriesLock") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        trivialOutLock.getPredicate,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"seriesLock does not correspond to seriesTxo"))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > invalid quantity to mint (non positive)") {
    val quantityInvalid: Int128 = BigInt(0)

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = valToTxo(seriesValue).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantityInvalid)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"quantity to mint must be positive"))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > invalid quantity to mint (non positive stxo)") {
    val quantity: Int128 = BigInt(10)
    val quantityInvalid: Int128 = BigInt(0)

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantity = quantityInvalid, tokenSupply = 5.some))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(s"quantity of input series constructor tokens must be positive"),
            UserInputError(s"quantity to mint must be less than total token supply available.")
          )
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > invalid quantity to mint (not a multiple of token supply)") {
    val quantity: Int128 = BigInt(10)
    val quantityInvalid: Int128 = BigInt(7)

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantity = quantity, tokenSupply = 5.some))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantityInvalid)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"quantity to mint must be a multiple of token supply"))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > invalid quantity to mint (exceeds available)") {
    val quantity: Int128 = BigInt(10)
    val quantityInvalid: Int128 = BigInt(55)

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantity = quantity, tokenSupply = 5.some))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantityInvalid)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"quantity to mint must be less than total token supply available."))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > invalid seriesId (fixedSeries provided)") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantityDescriptor = IMMUTABLE))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupVal = groupValue.copy(groupValue.getGroup.copy(fixedSeries = mockSeriesPolicyAlt.computeId.some))
    val groupTxo = valToTxo(groupVal).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"fixedSeries does not match provided Series ID"))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > IMMUTABLE quantity descriptor type") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantityDescriptor = IMMUTABLE))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesVal),
          UnspentTransactionOutput(trivialLockAddress, assetGroupSeriesImmutable),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > FRACTIONABLE quantity descriptor type") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantityDescriptor = FRACTIONABLE))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesVal),
          UnspentTransactionOutput(trivialLockAddress, assetGroupSeriesFractionable),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > ACCUMULATOR quantity descriptor type") {
    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesVal = seriesValue.copy(seriesValue.getSeries.copy(quantityDescriptor = ACCUMULATOR))
    val seriesTxo = valToTxo(seriesVal).copy(outputAddress = seriesAddr)

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = valToTxo(groupValue).copy(outputAddress = groupAddr)

    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(buildStxos(List(groupTxo, seriesTxo)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesVal),
          UnspentTransactionOutput(trivialLockAddress, assetGroupSeriesAccumulator),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(
        statement,
        groupTxo,
        seriesTxo,
        inPredicateLockFull,
        inPredicateLockFull,
        trivialLockAddress,
        None,
        None
      )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }
}
