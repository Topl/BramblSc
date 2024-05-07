package co.topl.brambl.builders

import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.LvlType
import co.topl.brambl.syntax.UnknownType
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.syntax.valueToTypeIdentifierSyntaxOps

class TransactionBuilderInterpreterGroupTransferSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildTransferAmountTransaction > unsupported token type (tokenIdentifier)") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(UnknownType)
      .run
    assertEquals(testTx, Left(UserInputErrors(Seq(UserInputError(s"UnknownType tokens are not supported.")))))
  }

  test("unsupported token type in txos is filtered out/ignored") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .withTxos(mockTxos :+ valToTxo(Value.defaultInstance)) // Value.empty
      .run
    val expectedTx = buildTransferAmountTransaction
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .withTxos(mockTxos) // The only difference is the unsupported txo is not present
      .run
    assert(
      (testTx.isRight && expectedTx.isRight) &&
      sortedTx(testTx.toOption.get).computeId == sortedTx(expectedTx.toOption.get).computeId
    )
  }

  test("buildTransferAmountTransaction > quantity to transfer is non positive") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .withAmount(0)
      .run
    assertEquals(testTx, Left(UserInputErrors(Seq(UserInputError(s"quantity to transfer must be positive")))))
  }

  test("buildTransferAmountTransaction > a txo isnt tied to lockPredicateFrom") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .withTxos(mockTxos :+ valToTxo(lvlValue, trivialLockAddress))
      .run
    assertEquals(
      testTx,
      Left(UserInputErrors(Seq(UserInputError(s"every lock in the txos must correspond to lockPredicateFrom"))))
    )
  }

  test("buildTransferAmountTransaction > non sufficient funds") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .withAmount(4)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(
            UserInputError(
              s"All tokens selected to transfer do not have enough funds to transfer. The desired quantity to transfer is 4 but the 2 tokens selected to transfer only have a combined quantity of 2."
            )
          )
        )
      )
    )
  }

  test("buildTransferAmountTransaction > fee not satisfied") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .withFee(3)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError(s"Not enough LVLs in input to satisfy fee"))
        )
      )
    )
  }

  test("buildTransferAmountTransaction > [complex] duplicate inputs are merged and split correctly") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        // to recipient
        buildRecipientUtxos(List(groupValue))
        ++
        // change due to excess fee and transfer input
        buildChangeUtxos(List(lvlValue, groupValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange.filterNot(v => List(LvlType, groupValue.value.typeIdentifier).contains(v.value.typeIdentifier))
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAmountTransaction > [simplest case] no change, only 1 output") {
    val txos = Seq(valToTxo(groupValue))
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .withTxos(txos)
      .withFee(0)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos(txos))
      .withOutputs(buildRecipientUtxos(List(groupValue)))
    assertEquals(testTx.toOption.get.computeId, expectedTx.computeId)
  }
}
