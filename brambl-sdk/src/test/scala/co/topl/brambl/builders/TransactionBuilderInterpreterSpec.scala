package co.topl.brambl.builders

import cats.Id
import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.MockHelpers
import co.topl.brambl.builders.TransactionBuilderApi.UnableToBuildTransaction
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.{AssetMintingStatement, Attestation, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.{
  groupPolicyAsGroupPolicySyntaxOps,
  ioTransactionAsTransactionSyntaxOps,
  seriesPolicyAsSeriesPolicySyntaxOps
}
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof}
import co.topl.brambl.models.box.FungibilityType.{GROUP, GROUP_AND_SERIES, SERIES}
import co.topl.genus.services.Txo
import co.topl.genus.services.TxoState.UNSPENT

class TransactionBuilderInterpreterSpec extends munit.FunSuite with MockHelpers {
  val txBuilder: TransactionBuilderApi[Id] = TransactionBuilderApi.make[Id](0, 0)

  test("buildSimpleLvlTransaction > No change") {
    val testTx = txBuilder.buildSimpleLvlTransaction(
      List(inputTxo),
      inPredicateLockFull,
      inPredicateLockFull,
      trivialLockAddress,
      1
    )
    assert(testTx.computeId == txFull.computeId)
  }

  test("buildSimpleLvlTransaction > With change") {
    val testTx = txBuilder.buildSimpleLvlTransaction(
      List(
        inputTxo.copy(
          transactionOutput = UnspentTransactionOutput(
            inLockFullAddress,
            Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(2).toByteArray))))
          )
        )
      ),
      inPredicateLockFull,
      trivialOutLock.getPredicate,
      trivialLockAddress,
      1
    )
    val expectedTx = txFull.copy(
      inputs = List(
        inputFull.copy(value =
          Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(2).toByteArray))))
        )
      ),
      outputs = List(output, output)
    )
    assert(testTx.computeId == expectedTx.computeId)
  }

  test("lvlOutput (Predicate)") {
    assert(
      txBuilder.lvlOutput(trivialOutLock.getPredicate, Int128(ByteString.copyFrom(BigInt(1).toByteArray))) == output
    )
  }

  test("lvlOutput (LockAddress)") {
    assert(txBuilder.lvlOutput(trivialLockAddress, Int128(ByteString.copyFrom(BigInt(1).toByteArray))) == output)
  }

  test("lockAddress") {
    assert(txBuilder.lockAddress(inLockFull) == inLockFullAddress)
  }

  test("datum") {
    val testDatum = txBuilder.datum()
    // Testing fields individually since the timestamp is generated at runtime
    assert(testDatum.event.metadata == txDatum.event.metadata)
    assert(testDatum.event.schedule.min == txDatum.event.schedule.min)
    assert(testDatum.event.schedule.max == txDatum.event.schedule.max)
  }

  test("unprovenAttestation") {
    assert(txBuilder.unprovenAttestation(inPredicateLockFull) == attFull)
  }

  test("buildSimpleGroupMintingTransaction > Success") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withGroupPolicies(Seq(Datum.GroupPolicy(mockGroupPolicy)))
      .withInputs(
        List(
          SpentTransactionOutput(
            mockGroupPolicy.registrationUtxo,
            Attestation().withPredicate(Attestation.Predicate(inPredicateLockFull, List(Proof()))),
            Value.defaultInstance.withLvl(Value.LVL(quantity))
          )
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(
            inLockFullAddress,
            Value.defaultInstance.withGroup(Value.Group(groupId = mockGroupPolicy.computeId, quantity = quantity))
          )
        )
      )
    val txRes = txBuilder.buildSimpleGroupMintingTransaction(
      inputTxo,
      inPredicateLockFull,
      mockGroupPolicy,
      quantity,
      inLockFullAddress
    )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleGroupMintingTransaction > invalid registrationTxo") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", dummyTxoAddress.copy(network = 10))
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockGroupPolicy,
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError("registrationTxo does not match registrationUtxo"))
        )
      )
    )
  }

  test("buildSimpleGroupMintingTransaction > invalid registrationUtxo") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        inputTxo.copy(transactionOutput =
          fullOutput.copy(value = Value.defaultInstance.withTopl(Value.TOPL(quantity)))
        ),
        inPredicateLockFull,
        mockGroupPolicy,
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError("registrationUtxo does not contain LVLs"))
        )
      )
    )
  }

  test("buildSimpleGroupMintingTransaction > invalid registrationLock") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        inputTxo,
        trivialOutLock.getPredicate,
        mockGroupPolicy,
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError("registrationLock does not correspond to registrationTxo"))
        )
      )
    )
  }

  test("buildSimpleGroupMintingTransaction > invalid quantityToMint") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(0).toByteArray))

    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockGroupPolicy,
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError("quantityToMint must be positive"))
        )
      )
    )
  }

  test("buildSimpleSeriesMintingTransaction > Success") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withSeriesPolicies(Seq(Datum.SeriesPolicy(mockSeriesPolicy)))
      .withInputs(
        List(
          SpentTransactionOutput(
            mockSeriesPolicy.registrationUtxo,
            Attestation().withPredicate(Attestation.Predicate(inPredicateLockFull, List(Proof()))),
            Value.defaultInstance.withLvl(Value.LVL(quantity))
          )
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(
            inLockFullAddress,
            Value.defaultInstance.withSeries(
              Value.Series(
                seriesId = mockSeriesPolicy.computeId,
                quantity = quantity,
                tokenSupply = mockSeriesPolicy.tokenSupply,
                quantityDescriptor = mockSeriesPolicy.quantityDescriptor,
                fungibility = mockSeriesPolicy.fungibility
              )
            )
          )
        )
      )
    val txRes = txBuilder.buildSimpleSeriesMintingTransaction(
      inputTxo,
      inPredicateLockFull,
      mockSeriesPolicy,
      quantity,
      inLockFullAddress
    )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleSeriesMintingTransaction > invalid registrationTxo") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress.copy(network = 10))
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockSeriesPolicy,
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError("registrationTxo does not match registrationUtxo"))
        )
      )
    )
  }

  test("buildSimpleSeriesMintingTransaction > invalid registrationUtxo") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        inputTxo.copy(transactionOutput =
          fullOutput.copy(value = Value.defaultInstance.withTopl(Value.TOPL(quantity)))
        ),
        inPredicateLockFull,
        mockSeriesPolicy,
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError("registrationUtxo does not contain LVLs"))
        )
      )
    )
  }

  test("buildSimpleSeriesMintingTransaction > invalid registrationLock") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        inputTxo,
        trivialOutLock.getPredicate,
        mockSeriesPolicy,
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError("registrationLock does not correspond to registrationTxo"))
        )
      )
    )
  }

  test("buildSimpleSeriesMintingTransaction > invalid quantityToMint") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(0).toByteArray))

    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockSeriesPolicy,
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError("quantityToMint must be positive"))
        )
      )
    )
  }

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

  test("buildSimpleAssetMintingTransaction > success, group and series fungible (IDs set, alloys unset)") {
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

  test("buildSimpleAssetMintingTransaction > fail, group fungible (currently not supported)") {
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
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError(s"Unsupported fungibility type. We currently only support GROUP_AND_SERIES"))
        )
      )
    )
  }

  test("buildSimpleAssetMintingTransaction > success, series fungible (currently not supported)") {
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
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError(s"Unsupported fungibility type. We currently only support GROUP_AND_SERIES"))
        )
      )
    )
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
        UnableToBuildTransaction(
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
        UnableToBuildTransaction(
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
        UnableToBuildTransaction(
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
        UnableToBuildTransaction(
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
        UnableToBuildTransaction(
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
        UnableToBuildTransaction(
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
        UnableToBuildTransaction(
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
        UnableToBuildTransaction(
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
        UnableToBuildTransaction(
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
        UnableToBuildTransaction(
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
        UnableToBuildTransaction(
          Seq(UserInputError(s"fixedSeries does not match provided Series ID"))
        )
      )
    )
  }
}
