package co.topl.brambl.builders

import cats.Id
import co.topl.brambl.MockHelpers
import co.topl.brambl.builders.TransactionBuilderApi.{UnableToBuildTransaction, UserInputError}
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.{Attestation, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.{
  groupPolicyAsGroupPolicySyntaxOps,
  ioTransactionAsTransactionSyntaxOps,
  seriesPolicyAsSeriesPolicySyntaxOps
}
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof}

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
          "Unable to build transaction to mint group constructor tokens",
          UserInputError("registrationTxo does not match registrationUtxo")
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
          "Unable to build transaction to mint group constructor tokens",
          UserInputError("registrationUtxo does not contain LVLs")
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
          "Unable to build transaction to mint group constructor tokens",
          UserInputError("registrationLock does not correspond to registrationTxo")
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
          "Unable to build transaction to mint group constructor tokens",
          UserInputError("quantityToMint must be positive")
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
          "Unable to build transaction to mint series constructor tokens",
          UserInputError("registrationTxo does not match registrationUtxo")
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
          "Unable to build transaction to mint series constructor tokens",
          UserInputError("registrationUtxo does not contain LVLs")
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
          "Unable to build transaction to mint series constructor tokens",
          UserInputError("registrationLock does not correspond to registrationTxo")
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
          "Unable to build transaction to mint series constructor tokens",
          UserInputError("quantityToMint must be positive")
        )
      )
    )
  }
}
