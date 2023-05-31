package co.topl.brambl.builders

import cats.Id
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import com.google.protobuf.ByteString
import quivr.models.Int128

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
}
