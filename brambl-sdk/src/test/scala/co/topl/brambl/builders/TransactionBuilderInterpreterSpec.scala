package co.topl.brambl.builders

import co.topl.brambl.builders.TransactionBuilderApi.BuildStxoError
import co.topl.brambl.{MockBifrostRpc, MockHelpers, MockWalletStateApi}
import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.models.{Datum, LockAddress, TransactionOutputAddress}
import co.topl.brambl.models.box.{Attestation, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.{groupPolicyAsGroupPolicySyntaxOps, ioTransactionAsTransactionSyntaxOps}
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

  test("buildSimpleGroupMintingTransaction > Success") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", TransactionOutputAddress(0, 0, 0, txFull.computeId))
    val quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withGroupPolicies(Seq(Datum.GroupPolicy(mockGroupPolicy)))
      .withInputs(
        List(
          SpentTransactionOutput(
            mockGroupPolicy.registrationUtxo,
            Attestation().withPredicate(Attestation.Predicate(trivialOutLock.getPredicate, List(Proof()))),
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
          .buildSimpleGroupMintingTransaction(MockWalletStateApi, MockBifrostRpc, mockGroupPolicy, inLockFull, quantity)
      } yield txRes match {
        case Right(testTx) => testTx.computeId == expectedTx.computeId
        case _             => false
      },
      true
    )
  }

  test("buildSimpleGroupMintingTransaction > fails if registrationUtxo's TX is not known by Bifrost RPC") {
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", TransactionOutputAddress(0, 0, 0, dummyTx.computeId))
    val testTx = txBuilder.buildSimpleGroupMintingTransaction(
      MockWalletStateApi,
      MockBifrostRpc,
      mockGroupPolicy,
      inLockFull,
      Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    )
    assertIO(testTx, Left(BuildStxoError(s"Could not retrieve TX with id ${dummyTx.computeId}")))
  }

  test("buildSimpleGroupMintingTransaction > fails if registrationUtxo's UTXO is not present in transaction") {
    // txFull only has 1 output
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", TransactionOutputAddress(0, 0, 1, txFull.computeId))
    val testTx = txBuilder.buildSimpleGroupMintingTransaction(
      MockWalletStateApi,
      MockBifrostRpc,
      mockGroupPolicy,
      inLockFull,
      Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    )
    assertIO(testTx, Left(BuildStxoError(s"Could not retrieve UTXO with index 1. Total Outputs: 1")))
  }

  test("buildSimpleGroupMintingTransaction > fails if registrationUtxo's Lock is not known in Wallet State") {
    // dummyTx is in MockBifrostRpc but not in walletState
    val mockGroupPolicy: GroupPolicy =
      GroupPolicy("Mock Group Policy", TransactionOutputAddress(0, 0, 0, txFullAlternative.computeId))
    val testTx = txBuilder.buildSimpleGroupMintingTransaction(
      MockWalletStateApi,
      MockBifrostRpc,
      mockGroupPolicy,
      inLockFull,
      Int128(ByteString.copyFrom(BigInt(1).toByteArray))
    )
    assertIO(testTx, Left(BuildStxoError(s"Could not retrieve Lock for Address $inLockFullAddress")))
  }
}
