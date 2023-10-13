package co.topl.brambl.builders

import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.{
  bigIntAsInt128,
  int128AsBigInt,
  ioTransactionAsTransactionSyntaxOps,
  valueToQuantitySyntaxOps,
  valueToTypeIdentifierSyntaxOps,
  LvlType
}

class TransactionBuilderInterpreterGroupTransferSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildTransferAmountTransaction > underlying error fails (unsupported token type)") {
    val testTx = buildTransferAmountTransaction
      .withTokenIdentifier(groupValue.value.typeIdentifier)
      .withTxos(mockTxos :+ valToTxo(Value.defaultInstance.withTopl(Value.TOPL(quantity))))
      .run
    assertEquals(testTx, Left(UserInputErrors(Seq(UserInputError(s"Invalid value type")))))
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
      Left(UserInputErrors(Seq(UserInputError(s"every lock does not correspond to fromLockAddr"))))
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
