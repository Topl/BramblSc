package co.topl.brambl.builders

import cats.Id
import co.topl.brambl.{MockDataApi, MockHelpers}
import co.topl.brambl.models.builders.{InputBuildRequest, OutputBuildRequest}
import co.topl.brambl.models.transaction.IoTransaction

class TransactionBuilderInterpreterSpec extends munit.FunSuite with MockHelpers {

  test("simplest case: 1 input, 1 output. Input and Tx data are available in DataApi") {
    // both LockAddress and Value are trivial for this test
    val outputRequests = List(OutputBuildRequest(trivialLockAddress, value))
    // Points to an existing Transaction Output that already has a lock and value associated with it
    val inputRequests = List(InputBuildRequest(dummyTxoAddress))
    val txBuilder: TransactionBuilder[Id] = TransactionBuilderInterpreter.make[Id](MockDataApi)
    val unprovenTx = txBuilder.constructUnprovenTransaction(inputRequests, outputRequests)

    // Verify no errors
    assert(unprovenTx.isRight)
    val res = unprovenTx.getOrElse(IoTransaction.defaultInstance)
    // Verify the transaction has the correct number of inputs and outputs
    assert(res.inputs.length == 1)
    assert(res.outputs.length == 1)
    // Verify the input has a lock and value
    assert(res.inputs.head.value.value.isDefined)
    assert(res.inputs.head.attestation.value.isDefined)
  }
}
