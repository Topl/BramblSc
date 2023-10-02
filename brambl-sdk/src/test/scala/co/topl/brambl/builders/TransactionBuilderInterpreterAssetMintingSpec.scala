package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.FungibilityType.{GROUP, GROUP_AND_SERIES, SERIES}
import co.topl.brambl.models.box.QuantityDescriptorType.{ACCUMULATOR, FRACTIONABLE, IMMUTABLE}
import co.topl.brambl.models.box.{AssetMintingStatement, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.{
  groupPolicyAsGroupPolicySyntaxOps,
  ioTransactionAsTransactionSyntaxOps,
  seriesPolicyAsSeriesPolicySyntaxOps
}
import co.topl.genus.services.Txo
import co.topl.genus.services.TxoState.UNSPENT
import com.google.protobuf.ByteString
import quivr.models.Int128

class TransactionBuilderInterpreterAssetMintingSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildSimpleAssetMintingTransaction > success, token supply unlimited (full series in output)") {
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(mockGroupPolicy.computeId.some, mockSeriesPolicy.computeId.some, quantity)
    )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, mint token supply quantity (series in output)") {
    val fullQuantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))
    val outQuantity = Int128(ByteString.copyFrom(BigInt(9).toByteArray))
    val mintedQuantity = Int128(ByteString.copyFrom(BigInt(5).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", 5.some, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, fullQuantity, 5.some))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesOut = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, outQuantity, 5.some))
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, fullQuantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, mintedQuantity)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(mockGroupPolicy.computeId.some, mockSeriesPolicy.computeId.some, mintedQuantity)
    )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesOut),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, mint multiple token supply quantity (series in output)") {
    val fullQuantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))
    val outQuantity = Int128(ByteString.copyFrom(BigInt(5).toByteArray))
    val mintedQuantity = Int128(ByteString.copyFrom(BigInt(25).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", 5.some, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, fullQuantity, 5.some))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesOut = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, outQuantity, 5.some))
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, fullQuantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, mintedQuantity)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(mockGroupPolicy.computeId.some, mockSeriesPolicy.computeId.some, mintedQuantity)
    )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesOut),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, mint ALL token supply quantity (series not in output)") {
    val fullQuantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))
    val mintedQuantity = Int128(ByteString.copyFrom(BigInt(50).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", 5.some, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, fullQuantity, 5.some))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, fullQuantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, mintedQuantity)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(mockGroupPolicy.computeId.some, mockSeriesPolicy.computeId.some, mintedQuantity)
    )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, group and series fungible") {
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, seriesAddr, fungibility = GROUP_AND_SERIES)
    val seriesValue = Value.defaultInstance.withSeries(
      Value.Series(mockSeriesPolicy.computeId, quantity, fungibility = GROUP_AND_SERIES)
    )
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(mockGroupPolicy.computeId.some, mockSeriesPolicy.computeId.some, quantity, None, None)
    )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, group fungible") {
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr, fungibility = GROUP)
    val seriesValue =
      Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, fungibility = GROUP))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(mockGroupPolicy.computeId.some, mockSeriesPolicy.computeId.some, quantity, None, None, GROUP)
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    assert(testTx.isRight && testTx.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, series fungible") {
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr, fungibility = SERIES)
    val seriesValue =
      Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, fungibility = SERIES))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(mockGroupPolicy.computeId.some, mockSeriesPolicy.computeId.some, quantity, None, None, SERIES)
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    assert(testTx.isRight && testTx.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > success, fixedSeries provided") {
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val fixedSeries = mockSeriesPolicy.computeId.some
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr, fixedSeries)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity, fixedSeries))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(mockGroupPolicy.computeId.some, mockSeriesPolicy.computeId.some, quantity)
    )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > invalid groupTxo") {
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, dummyTxoAddress)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, dummyTxoAddress)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, value), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, value), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = trivialOutLock.getPredicate

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = trivialOutLock.getPredicate

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))
    val quantityInvalid = Int128(ByteString.copyFrom(BigInt(0).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantityInvalid)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))
    val quantityInvalid = Int128(ByteString.copyFrom(BigInt(0).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", 5.some, seriesAddr)
    val seriesValue =
      Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantityInvalid, 5.some))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))
    val quantityInvalid = Int128(ByteString.copyFrom(BigInt(7).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", 5.some, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, 5.some))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantityInvalid)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))
    val quantityInvalid = Int128(ByteString.copyFrom(BigInt(55).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", 5.some, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, 5.some))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantityInvalid)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr)
    val seriesValue = Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val fixedSeries = mockSeriesPolicy.copy("different series").computeId.some
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr, fixedSeries)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity, fixedSeries))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)

    val testTx = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
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
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr, IMMUTABLE)
    val seriesValue =
      Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None, IMMUTABLE))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(
        mockGroupPolicy.computeId.some,
        mockSeriesPolicy.computeId.some,
        quantity,
        quantityDescriptor = IMMUTABLE
      )
    )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > FRACTIONABLE quantity descriptor type") {
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr, FRACTIONABLE)
    val seriesValue =
      Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None, FRACTIONABLE))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(
        mockGroupPolicy.computeId.some,
        mockSeriesPolicy.computeId.some,
        quantity,
        quantityDescriptor = FRACTIONABLE
      )
    )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleAssetMintingTransaction > ACCUMULATOR quantity descriptor type") {
    val quantity = Int128(ByteString.copyFrom(BigInt(10).toByteArray))

    val seriesAddr = dummyTxoAddress.copy(index = 1)
    val mockSeriesPolicy: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, seriesAddr, ACCUMULATOR)
    val seriesValue =
      Value.defaultInstance.withSeries(Value.Series(mockSeriesPolicy.computeId, quantity, None, ACCUMULATOR))
    val seriesTxo = Txo(UnspentTransactionOutput(inLockFullAddress, seriesValue), UNSPENT, seriesAddr)
    val seriesLock = inPredicateLockFull

    val groupAddr = dummyTxoAddress.copy(index = 2)
    val mockGroupPolicy: GroupPolicy = GroupPolicy("Mock Group Policy", groupAddr)
    val groupValue = Value.defaultInstance.withGroup(Value.Group(mockGroupPolicy.computeId, quantity))
    val groupTxo = Txo(UnspentTransactionOutput(inLockFullAddress, groupValue), UNSPENT, groupAddr)
    val groupLock = inPredicateLockFull

    val outAddr = trivialLockAddress
    val statement: AssetMintingStatement = AssetMintingStatement(groupAddr, seriesAddr, quantity)
    val mintedValue = Value.defaultInstance.withAsset(
      Value.Asset(
        mockGroupPolicy.computeId.some,
        mockSeriesPolicy.computeId.some,
        quantity,
        quantityDescriptor = ACCUMULATOR
      )
    )

    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMintingStatements(Seq(statement))
      .withInputs(
        List(
          SpentTransactionOutput(groupAddr, attFull, groupValue),
          SpentTransactionOutput(seriesAddr, attFull, seriesValue)
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, seriesValue),
          UnspentTransactionOutput(trivialLockAddress, mintedValue),
          UnspentTransactionOutput(inLockFullAddress, groupValue)
        )
      )
    val txRes = txBuilder
      .buildSimpleAssetMintingTransaction(statement, groupTxo, seriesTxo, groupLock, seriesLock, outAddr, None, None)
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }
}
