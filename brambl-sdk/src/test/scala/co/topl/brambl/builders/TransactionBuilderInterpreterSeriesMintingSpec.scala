package co.topl.brambl.builders

import co.topl.brambl.models.Event.SeriesPolicy
import co.topl.brambl.models.box.{Attestation, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.Datum
import co.topl.brambl.syntax.{bigIntAsInt128, ioTransactionAsTransactionSyntaxOps, seriesPolicyAsSeriesPolicySyntaxOps}
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof}

class TransactionBuilderInterpreterSeriesMintingSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildSimpleSeriesMintingTransaction > Success") {
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withSeriesPolicies(Seq(Datum.SeriesPolicy(mockSeriesPolicy)))
      .withInputs(buildStxos(List(valToTxo(lvlValue))))
      .withOutputs(List(valToUtxo(seriesValue)))
    val txRes = txBuilder.buildSimpleSeriesMintingTransaction(
      valToTxo(lvlValue),
      inPredicateLockFull,
      mockSeriesPolicy,
      quantity,
      inLockFullAddress
    )
    assert(txRes.isRight && txRes.toOption.get.computeId == expectedTx.computeId)
  }

  test("buildSimpleSeriesMintingTransaction > invalid registrationTxo") {
    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        valToTxo(lvlValue),
        inPredicateLockFull,
        mockSeriesPolicy.copy(registrationUtxo = dummyTxoAddress.copy(network = 10)),
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

  test("buildSimpleSeriesMintingTransaction > invalid registrationUtxo") {
    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        valToTxo(Value.defaultInstance.withTopl(Value.TOPL(quantity))),
        inPredicateLockFull,
        mockSeriesPolicy,
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

  test("buildSimpleSeriesMintingTransaction > invalid registrationLock") {
    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        valToTxo(lvlValue),
        trivialOutLock.getPredicate,
        mockSeriesPolicy,
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

  test("buildSimpleSeriesMintingTransaction > invalid quantityToMint") {
    val testTx = txBuilder
      .buildSimpleSeriesMintingTransaction(
        valToTxo(lvlValue),
        inPredicateLockFull,
        mockSeriesPolicy,
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
