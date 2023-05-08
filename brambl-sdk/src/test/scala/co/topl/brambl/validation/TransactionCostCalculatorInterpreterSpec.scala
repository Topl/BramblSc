package co.topl.brambl.validation

import cats.Id
import co.topl.brambl.MockHelpers
import co.topl.brambl.common.ContainsImmutable.instances.ioTransactionImmutable
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.SpentTransactionOutput

class TransactionCostCalculatorInterpreterSpec extends munit.FunSuite with MockHelpers {

  test("cost an empty transaction") {
    val tx = IoTransaction.defaultInstance
    val calculator = TransactionCostCalculatorInterpreter.make[Id](TransactionCostConfig())
    val result = calculator.costOf(tx)
    assertEquals(result, 1L)
  }

  test("cost a transaction with schedule") {
    val tx = dummyTx
    val calculator = TransactionCostCalculatorInterpreter.make[Id](TransactionCostConfig())
    val result = calculator.costOf(tx)
    assertEquals(result, expectedDataCost(tx))
  }

  test("cost a transaction with schedule and outputs") {
    val tx = dummyTx.addOutputs(output)
    val calculator = TransactionCostCalculatorInterpreter.make[Id](TransactionCostConfig())
    val result = calculator.costOf(tx)
    assertEquals(result, expectedDataCost(tx) + 5L)
  }

  test("cost a transaction with schedule, inputs, and outputs") {
    val tx = txFull.clearInputs.addInputs(SpentTransactionOutput(dummyTxoAddress, nonEmptyAttestation, value))
    val calculator = TransactionCostCalculatorInterpreter.make[Id](TransactionCostConfig())
    val result = calculator.costOf(tx)
    assertEquals(
      result,
      expectedDataCost(tx) +
      // Cost of 1 output
      5L -
      // Reward of 1 input
      1L +
      // Cost of locked proof
      1L +
      // Cost of digest proof
      50L + 50L +
      // Cost of signature proof
      50L + 100L +
      // Cost of height proof
      50L + 5L +
      // Cost of tick proof
      50L + 5L
    )
  }

  private def expectedDataCost(tx: IoTransaction): Long = {
    val bytes = ioTransactionImmutable.immutableBytes(tx).value
    bytes.size() / 1024L + 1L
  }
}
