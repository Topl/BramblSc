package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.builders.TransactionBuilderApi.{UnableToBuildTransaction, UserInputError}
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.Datum
import co.topl.brambl.models.box.{Attestation, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.{
  groupPolicyAsGroupPolicySyntaxOps,
  ioTransactionAsTransactionSyntaxOps,
  seriesPolicyAsSeriesPolicySyntaxOps
}
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof}
import munit.CatsEffectAssertions.assertIO

class TransactionBuilderInterpreterSpec extends munit.FunSuite with MockHelpers {
  val txBuilder: TransactionBuilderApi[F] = TransactionBuilderApi.make[F](0, 0)

  test("buildSimpleLvlTransaction > No change") {
    val testTx = txBuilder.buildSimpleLvlTransaction(
      List(inputTxo),
      inPredicateLockFull,
      inPredicateLockFull,
      trivialLockAddress,
      1
    )
    assertIO(testTx.map(_.computeId), txFull.computeId)
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
    assertIO(testTx.map(_.computeId), expectedTx.computeId)
  }

  test("lvlOutput (Predicate)") {
    assertIO(
      txBuilder.lvlOutput(trivialOutLock.getPredicate, Int128(ByteString.copyFrom(BigInt(1).toByteArray))),
      output
    )
  }

  test("lvlOutput (LockAddress)") {
    assertIO(txBuilder.lvlOutput(trivialLockAddress, Int128(ByteString.copyFrom(BigInt(1).toByteArray))), output)
  }

  test("lockAddress") {
    assertIO(txBuilder.lockAddress(inLockFull), inLockFullAddress)
  }

  test("datum") {
    assertIO(
      for {
        testDatum <- txBuilder.datum().map(_.event)
      } yield // Testing fields individually since the timestamp is generated at runtime
        (testDatum.metadata == txDatum.event.metadata) && (testDatum.schedule.min == txDatum.event.schedule.min) && (testDatum.schedule.max == txDatum.event.schedule.max),
      true
    )
  }

  test("unprovenAttestation") {
    assertIO(txBuilder.unprovenAttestation(inPredicateLockFull), attFull)
  }

  test("buildSimpleGroupMintingTransaction > Success, no change (fee specified)") {
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
    assertIO(
      for {
        txRes <- txBuilder
          .buildSimpleGroupMintingTransaction(
            inputTxo,
            inPredicateLockFull,
            mockGroupPolicy,
            quantity,
            inLockFullAddress,
            trivialLockAddress.some,
            quantity.some
          )
      } yield txRes match {
        case Right(testTx) => testTx.computeId == expectedTx.computeId
        case _             => false
      },
      true
    )
  }

  test("buildSimpleGroupMintingTransaction > Success, no change (fee unspecified)") {
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
    assertIO(
      for {
        txRes <- txBuilder
          .buildSimpleGroupMintingTransaction(
            inputTxo,
            inPredicateLockFull,
            mockGroupPolicy,
            quantity,
            inLockFullAddress
          )
      } yield txRes match {
        case Right(testTx) => testTx.computeId == expectedTx.computeId
        case _             => false
      },
      true
    )
  }

  test("buildSimpleGroupMintingTransaction > Success, with change") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", dummyTxoAddress)
    val quantity3 = Int128(ByteString.copyFrom(BigInt(3).toByteArray))
    val quantity2 = Int128(ByteString.copyFrom(BigInt(2).toByteArray))
    val quantity1 = Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withGroupPolicies(Seq(Datum.GroupPolicy(mockGroupPolicy)))
      .withInputs(
        List(
          SpentTransactionOutput(
            mockGroupPolicy.registrationUtxo,
            Attestation().withPredicate(Attestation.Predicate(inPredicateLockFull, List(Proof()))),
            Value.defaultInstance.withLvl(Value.LVL(quantity3))
          )
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(
            trivialLockAddress,
            Value.defaultInstance.withLvl(Value.LVL(quantity = quantity1))
          ),
          UnspentTransactionOutput(
            inLockFullAddress,
            Value.defaultInstance.withGroup(Value.Group(groupId = mockGroupPolicy.computeId, quantity = quantity1))
          )
        )
      )
    assertIO(
      for {
        txRes <- txBuilder
          .buildSimpleGroupMintingTransaction(
            inputTxo.copy(transactionOutput =
              fullOutput.copy(value = Value.defaultInstance.withLvl(Value.LVL(quantity3)))
            ),
            inPredicateLockFull,
            mockGroupPolicy,
            quantity1,
            inLockFullAddress,
            trivialLockAddress.some,
            quantity2.some
          )
      } yield txRes match {
        case Right(testTx) => testTx.computeId == expectedTx.computeId
        case _             => false
      },
      true
    )
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
    assertIO(
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
    assertIO(
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
    assertIO(
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
    assertIO(
      testTx,
      Left(
        UnableToBuildTransaction(
          "Unable to build transaction to mint group constructor tokens",
          UserInputError("quantityToMint must be positive")
        )
      )
    )
  }

  test("buildSimpleGroupMintingTransaction > changeLockAddress specified while quantityForFee not") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockGroupPolicy,
        quantity,
        inLockFullAddress,
        inLockFullAddress.some
      )
    assertIO(
      testTx,
      Left(
        UnableToBuildTransaction(
          "Unable to build transaction to mint group constructor tokens",
          UserInputError("changeLockAddress and quantityForFee must be both specified or both unspecified")
        )
      )
    )
  }

  test("buildSimpleGroupMintingTransaction > quantityForFee specified while changeLockAddress not") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockGroupPolicy,
        quantity,
        inLockFullAddress,
        None,
        quantity.some
      )
    assertIO(
      testTx,
      Left(
        UnableToBuildTransaction(
          "Unable to build transaction to mint group constructor tokens",
          UserInputError("changeLockAddress and quantityForFee must be both specified or both unspecified")
        )
      )
    )
  }

  test("buildSimpleGroupMintingTransaction > invalid quantityForFee (non-positive)") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    val quantity0 = Int128(ByteString.copyFrom(BigInt(0).toByteArray))

    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockGroupPolicy,
        quantity,
        inLockFullAddress,
        inLockFullAddress.some,
        quantity0.some
      )
    assertIO(
      testTx,
      Left(
        UnableToBuildTransaction(
          "Unable to build transaction to mint group constructor tokens",
          UserInputError("If specified, quantityForFee must be positive")
        )
      )
    )
  }

  test("buildSimpleGroupMintingTransaction > invalid quantityForFee (exceeds input)") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    val quantity2 = Int128(ByteString.copyFrom(BigInt(2).toByteArray))

    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockGroupPolicy,
        quantity,
        inLockFullAddress,
        inLockFullAddress.some,
        quantity2.some
      )
    assertIO(
      testTx,
      Left(
        UnableToBuildTransaction(
          "Unable to build transaction to mint group constructor tokens",
          UserInputError("If specified, quantityForFee must not exceed the LVLs contained in the registrationUtxo")
        )
      )
    )
  }

  test("buildSimpleSeriesMintingTransaction > Success, no change (fee specified)") {
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
    assertIO(
      for {
        txRes <- txBuilder
          .buildSimpleSeriesMintingTransaction(
            inputTxo,
            inPredicateLockFull,
            mockSeriesPolicy,
            quantity,
            inLockFullAddress,
            trivialLockAddress.some,
            quantity.some
          )
      } yield txRes match {
        case Right(testTx) => testTx.computeId == expectedTx.computeId
        case _             => false
      },
      true
    )
  }

  test("buildSimpleSeriesMintingTransaction > Success, no change (fee unspecified)") {
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
    assertIO(
      for {
        txRes <- txBuilder
          .buildSimpleSeriesMintingTransaction(
            inputTxo,
            inPredicateLockFull,
            mockSeriesPolicy,
            quantity,
            inLockFullAddress
          )
      } yield txRes match {
        case Right(testTx) => testTx.computeId == expectedTx.computeId
        case _             => false
      },
      true
    )
  }

  test("buildSimpleSeriesMintingTransaction > Success, with change") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress)
    val quantity3 = Int128(ByteString.copyFrom(BigInt(3).toByteArray))
    val quantity2 = Int128(ByteString.copyFrom(BigInt(2).toByteArray))
    val quantity1 = Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withSeriesPolicies(Seq(Datum.SeriesPolicy(mockSeriesPolicy)))
      .withInputs(
        List(
          SpentTransactionOutput(
            mockSeriesPolicy.registrationUtxo,
            Attestation().withPredicate(Attestation.Predicate(inPredicateLockFull, List(Proof()))),
            Value.defaultInstance.withLvl(Value.LVL(quantity3))
          )
        )
      )
      .withOutputs(
        List(
          UnspentTransactionOutput(
            trivialLockAddress,
            Value.defaultInstance.withLvl(Value.LVL(quantity = quantity1))
          ),
          UnspentTransactionOutput(
            inLockFullAddress,
            Value.defaultInstance.withSeries(
              Value.Series(
                seriesId = mockSeriesPolicy.computeId,
                quantity = quantity1,
                tokenSupply = mockSeriesPolicy.tokenSupply,
                quantityDescriptor = mockSeriesPolicy.quantityDescriptor,
                fungibility = mockSeriesPolicy.fungibility
              )
            )
          )
        )
      )
    assertIO(
      for {
        txRes <- txBuilder
          .buildSimpleSeriesMintingTransaction(
            inputTxo.copy(transactionOutput =
              fullOutput.copy(value = Value.defaultInstance.withLvl(Value.LVL(quantity3)))
            ),
            inPredicateLockFull,
            mockSeriesPolicy,
            quantity1,
            inLockFullAddress,
            trivialLockAddress.some,
            quantity2.some
          )
      } yield txRes match {
        case Right(testTx) => testTx.computeId == expectedTx.computeId
        case _             => false
      },
      true
    )
  }

  test("buildSimpleSeriesMintingTransaction > invalid registrationTxo") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockSeriesPolicy,
        quantity,
        inLockFullAddress
      )
    assertIO(
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
    assertIO(
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
    assertIO(
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
    assertIO(
      testTx,
      Left(
        UnableToBuildTransaction(
          "Unable to build transaction to mint series constructor tokens",
          UserInputError("quantityToMint must be positive")
        )
      )
    )
  }

  test("buildSimpleSeriesMintingTransaction > changeLockAddress specified while quantityForFee not") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockSeriesPolicy,
        quantity,
        inLockFullAddress,
        inLockFullAddress.some
      )
    assertIO(
      testTx,
      Left(
        UnableToBuildTransaction(
          "Unable to build transaction to mint series constructor tokens",
          UserInputError("changeLockAddress and quantityForFee must be both specified or both unspecified")
        )
      )
    )
  }

  test("buildSimpleSeriesMintingTransaction > quantityForFee specified while changeLockAddress not") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))

    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockSeriesPolicy,
        quantity,
        inLockFullAddress,
        None,
        quantity.some
      )
    assertIO(
      testTx,
      Left(
        UnableToBuildTransaction(
          "Unable to build transaction to mint series constructor tokens",
          UserInputError("changeLockAddress and quantityForFee must be both specified or both unspecified")
        )
      )
    )
  }

  test("buildSimpleSeriesMintingTransaction > invalid quantityForFee (non-positive)") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    val quantity0 = Int128(ByteString.copyFrom(BigInt(0).toByteArray))

    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockSeriesPolicy,
        quantity,
        inLockFullAddress,
        inLockFullAddress.some,
        quantity0.some
      )
    assertIO(
      testTx,
      Left(
        UnableToBuildTransaction(
          "Unable to build transaction to mint series constructor tokens",
          UserInputError("If specified, quantityForFee must be positive")
        )
      )
    )
  }

  test("buildSimpleSeriesMintingTransaction > invalid quantityForFee (exceeds input)") {
    val mockSeriesPolicy: SeriesPolicy =
      SeriesPolicy("Mock Series Policy", None, dummyTxoAddress)
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    val quantity2 = Int128(ByteString.copyFrom(BigInt(2).toByteArray))

    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        inputTxo,
        inPredicateLockFull,
        mockSeriesPolicy,
        quantity,
        inLockFullAddress,
        inLockFullAddress.some,
        quantity2.some
      )
    assertIO(
      testTx,
      Left(
        UnableToBuildTransaction(
          "Unable to build transaction to mint series constructor tokens",
          UserInputError("If specified, quantityForFee must not exceed the LVLs contained in the registrationUtxo")
        )
      )
    )
  }
}
