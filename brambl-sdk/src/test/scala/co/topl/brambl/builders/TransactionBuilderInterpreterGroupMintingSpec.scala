package co.topl.brambl.builders

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.Datum
import co.topl.brambl.models.box.Value
import co.topl.brambl.syntax.{ioTransactionAsTransactionSyntaxOps, valueToTypeIdentifierSyntaxOps, LvlType}

class TransactionBuilderInterpreterGroupMintingSpec extends TransactionBuilderInterpreterSpecBase {

  test("Success") {
    val txRes = buildMintGroupTransaction.run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withGroupPolicies(Seq(Datum.GroupPolicy(mockGroupPolicyAlt)))
      .withInputs(buildStxos(mockTxos :+ valToTxo(lvlValue, txAddr = mockGroupPolicyAlt.registrationUtxo)))
      .withOutputs(
        // minted output
        buildRecipientUtxos(List(groupValueAlt))
        ++
        // fee change
        buildChangeUtxos(List(lvlValue))
        ++
        // non-lvl change (i.e, unaffected by fee and registration)
        buildChangeUtxos(mockChange.filterNot(_.value.typeIdentifier == LvlType))
      )
    assert(txRes.isRight && sortedTx(txRes.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("input txos do not contain registrationUtxo") {
    val testTx = buildMintGroupTransaction
      .withPolicy(mockGroupPolicy.copy(registrationUtxo = dummyTxoAddress.copy(network = 10)))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("Input TXOs need to contain exactly one txo matching the registrationUtxo"))
        )
      )
    )
  }

  test("unsupported token type in txos is filtered out/ignored") {
    val testTx = buildMintGroupTransaction
      .addTxo(valToTxo(Value.defaultInstance)) // Value.empty
      .run
    val expectedTx = buildMintGroupTransaction.run // The only difference is the unsupported txo is not present
    assert(
      (testTx.isRight && expectedTx.isRight) &&
      sortedTx(testTx.toOption.get).computeId == sortedTx(expectedTx.toOption.get).computeId
    )
  }

  test("registrationUtxo does not contain lvls") {
    val newAddr = dummyTxoAddress.copy(network = 10)
    val testTx = buildMintGroupTransaction
      .addTxo(valToTxo(groupValue, txAddr = newAddr))
      .withPolicy(mockGroupPolicy.copy(registrationUtxo = newAddr))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("registrationUtxo does not contain LVLs"))
        )
      )
    )
  }

  test("all txos do not have the right lock predicate") {
    val testTx = buildMintGroupTransaction
      .addTxo(valToTxo(groupValue, trivialLockAddress))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("every lock in the txos must correspond to lockPredicateFrom"))
        )
      )
    )
  }

  test("quantity to mint is non-positive") {
    val testTx = buildMintGroupTransaction
      .withMintAmount(0)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("quantityToMint must be positive"))
        )
      )
    )
  }

  test("not enough lvls for fee") {
    val testTx = buildMintGroupTransaction
      .withFee(4)
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
}
