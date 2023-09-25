package co.topl.brambl.validation

import cats.Id
import cats.implicits._
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{Datum, Event, TransactionOutputAddress}
import co.topl.brambl.syntax._
import scala.language.implicitConversions

/**
 * Test to coverage this specific syntax validation:
 *  - Validations only for minting a Proposal Updated
 *  -
 */
class TransactionSyntaxInterpreterMintingCaseProposalUpdateSpec extends munit.FunSuite with MockHelpers {

  private val txoAddress_1 = TransactionOutputAddress(1, 0, 0, dummyTxIdentifier)
  private val txoAddress_2 = TransactionOutputAddress(2, 0, 0, dummyTxIdentifier)

  test("Valid data-input case 1, minting a proposal updated Token") {
    val value_1_in: Value =
      Value.defaultInstance.withTopl(
        Value.TOPL(
          quantity = BigInt(1),
          registration = None
        )
      )

    val value_1_out: Value =
      Value.defaultInstance.withUpdateProposal(
        Value.UpdateProposal(
          label = "Proposal update 1",
          vrfPrecision = Some(1)
        )
      )

    val value_2_out: Value =
      Value.defaultInstance.withTopl(
        Value.TOPL(
          quantity = BigInt(1),
          registration = None
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val output_1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_1_out)
    val output_2: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_2_out)

    val testTx = txFull.copy(
      inputs = List(input_1),
      outputs = List(output_1, output_2)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    assertEquals(result.map(_.toList.size).getOrElse(0), 0)
  }

}
