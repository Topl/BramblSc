package co.topl.brambl.builders

import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.Datum
import co.topl.brambl.syntax.{bigIntAsInt128, ioTransactionAsTransactionSyntaxOps}

class TransactionBuilderInterpreterGroupMintingSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildSimpleGroupMintingTransaction > Success") {
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withGroupPolicies(Seq(Datum.GroupPolicy(mockGroupPolicy)))
      .withInputs(buildStxos(List(valToTxo(lvlValue))))
      .withOutputs(List(valToUtxo(groupValue)))
    val txRes = txBuilder.buildSimpleGroupMintingTransaction(
      valToTxo(lvlValue),
      inPredicateLockFull,
      mockGroupPolicy,
      quantity,
      inLockFullAddress
    )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleGroupMintingTransaction > invalid registrationTxo") {
    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        valToTxo(lvlValue),
        inPredicateLockFull,
        mockGroupPolicy.copy(registrationUtxo = dummyTxoAddress.copy(network = 10)),
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("registrationTxo does not match registrationUtxo"))
        )
      )
    )
  }

  test("buildSimpleGroupMintingTransaction > invalid registrationUtxo") {
    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        valToTxo(Value.defaultInstance.withTopl(Value.TOPL(quantity))),
        inPredicateLockFull,
        mockGroupPolicy,
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("registrationUtxo does not contain LVLs"))
        )
      )
    )
  }

  test("buildSimpleGroupMintingTransaction > invalid registrationLock") {
    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        valToTxo(lvlValue),
        trivialOutLock.getPredicate,
        mockGroupPolicy,
        quantity,
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("registrationLock does not correspond to registrationTxo"))
        )
      )
    )
  }

  test("buildSimpleGroupMintingTransaction > invalid quantityToMint") {
    val testTx = txBuilder
      .buildSimpleGroupMintingTransaction(
        valToTxo(lvlValue),
        inPredicateLockFull,
        mockGroupPolicy,
        BigInt(0),
        inLockFullAddress
      )
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("quantityToMint must be positive"))
        )
      )
    )
  }
}
