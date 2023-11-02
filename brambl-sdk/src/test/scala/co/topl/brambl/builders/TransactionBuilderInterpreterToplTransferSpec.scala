package co.topl.brambl.builders

import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.{ioTransactionAsTransactionSyntaxOps, valueToTypeIdentifierSyntaxOps, LvlType}

class TransactionBuilderInterpreterToplTransferSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildTransferAmountTransaction > unsupported token type (txos)") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(toplValue.value.typeIdentifier)
      .withTxos(mockTxos :+ valToTxo(Value.defaultInstance)) // Value.empty
      .run
    assertEquals(testTx, Left(UserInputErrors(Seq(UserInputError(s"UnknownType tokens are not supported.")))))
  }

  test("buildTransferAmountTransaction > Topl with staking registration") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(toplReg1.value.typeIdentifier)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(Seq(UserInputError(s"If tokenIdentifier is a Topl type, staking registration must be None")))
      )
    )
  }

  test("buildTransferAmountTransaction > quantity to transfer is non positive") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(toplValue.value.typeIdentifier)
      .withAmount(0)
      .run
    assertEquals(testTx, Left(UserInputErrors(Seq(UserInputError(s"quantity to transfer must be positive")))))
  }

  test("buildTransferAmountTransaction > a txo isnt tied to lockPredicateFrom") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(toplValue.value.typeIdentifier)
      .withTxos(mockTxos :+ valToTxo(lvlValue, trivialLockAddress))
      .run
    assertEquals(
      testTx,
      Left(UserInputErrors(Seq(UserInputError(s"every lock in the txos must correspond to lockPredicateFrom"))))
    )
  }

  test("buildTransferAmountTransaction > non sufficient funds") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(toplValue.value.typeIdentifier)
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
      .withTokenIdentifier(toplValue.value.typeIdentifier)
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
      .withTokenIdentifier(toplValue.value.typeIdentifier)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos())
      .withOutputs(
        // to recipient
        buildRecipientUtxos(List(toplValue))
        ++
        // change due to excess fee and transfer input
        buildChangeUtxos(List(lvlValue, toplValue))
        ++
        // change values unaffected by recipient transfer and fee
        buildChangeUtxos(
          mockChange.filterNot(v => List(LvlType, toplValue.value.typeIdentifier).contains(v.value.typeIdentifier))
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildTransferAmountTransaction > [simplest case] no change, only 1 output") {
    val txos = Seq(valToTxo(toplValue))
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(toplValue.value.typeIdentifier)
      .withTxos(txos)
      .withFee(0)
      .run
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(buildStxos(txos))
      .withOutputs(buildRecipientUtxos(List(toplValue)))
    assertEquals(testTx.toOption.get.computeId, expectedTx.computeId)
  }
}
