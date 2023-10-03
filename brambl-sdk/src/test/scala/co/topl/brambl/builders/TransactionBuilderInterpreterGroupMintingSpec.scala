package co.topl.brambl.builders

import co.topl.brambl.builders.TransactionBuilderApi.UnableToBuildTransaction
import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.models.box.{Attestation, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.Datum
import co.topl.brambl.syntax.{groupPolicyAsGroupPolicySyntaxOps, ioTransactionAsTransactionSyntaxOps}
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof}

class TransactionBuilderInterpreterGroupMintingSpec extends TransactionBuilderInterpreterSpecBase {

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
}
