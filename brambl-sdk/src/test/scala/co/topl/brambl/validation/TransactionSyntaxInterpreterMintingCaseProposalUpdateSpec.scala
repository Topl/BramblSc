package co.topl.brambl.validation

import cats.Id
import cats.implicits._
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.box.{UpdateProposalMintingStatement, Value}
import co.topl.brambl.models.transaction.{SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{Datum, Event, TransactionOutputAddress}
import co.topl.brambl.syntax._
import co.topl.consensus.models.{SignatureKesProduct, SignatureKesSum, StakingAddress, StakingRegistration}
import com.google.protobuf.ByteString
import scala.language.implicitConversions

/**
 * Test to coverage this specific syntax validation:
 *  - Validations only for minting a Proposal Updated
 *  -
 */
class TransactionSyntaxInterpreterMintingCaseProposalUpdateSpec extends munit.FunSuite with MockHelpers {

  private val txoAddress_1 = TransactionOutputAddress(1, 0, 0, dummyTxIdentifier)

  // models generators defined on Node. We can not use them on brambl
  val signature = SignatureKesProduct(
    superSignature = SignatureKesSum(
      ByteString.copyFrom(Array.fill(32)(0: Byte)),
      ByteString.copyFrom(Array.fill(64)(0: Byte)),
      Seq.empty
    ),
    subSignature = SignatureKesSum(
      ByteString.copyFrom(Array.fill(32)(0: Byte)),
      ByteString.copyFrom(Array.fill(64)(0: Byte)),
      Seq.empty
    ),
    subRoot = ByteString.copyFrom(Array.fill(32)(0: Byte))
  )

  /**
   * There is no proposal update statements
   * None registration
   */
  test("Invalid data-input case 1, minting a proposal updated Token") {
    // registration is None
    val value_1_in: Value =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = None))

    val value_1_out: Value =
      Value.defaultInstance.withUpdateProposal(
        Value.UpdateProposal(label = "Proposal update 1", vrfPrecision = Some(1))
      )

    val value_2_out: Value =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = None))

    val inputs = List(SpentTransactionOutput(txoAddress_1, attFull, value_1_in))
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs)

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(TransactionSyntaxError.InvalidUpdateProposal(Seq(value_1_out.getUpdateProposal)))
    )
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }

  /**
   * There is no proposal update statements
   * None registration
   * quantity = 0
   */
  test("Invalid data-input case 2, minting a proposal updated Token") {
    val value_1_in: Value =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(0), registration = None))

    val value_1_out: Value =
      Value.defaultInstance.withUpdateProposal(
        Value.UpdateProposal(label = "Proposal update 1", vrfPrecision = Some(-1))
      )

    val inputs = List(SpentTransactionOutput(txoAddress_1, attFull, value_1_in))
    val outputs = List(UnspentTransactionOutput(trivialLockAddress, value_1_out))

    val testTx = txFull.copy(inputs = inputs, outputs = outputs)

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(TransactionSyntaxError.InvalidUpdateProposal(Seq(value_1_out.getUpdateProposal)))
    )
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }

  /**
   * There is no proposal update statements
   */
  test("Invalid data-input case 3, minting a proposal updated Token") {

    val address = StakingAddress(ByteString.copyFrom(Array.fill(32)(0: Byte)))
    val stakingRegistration = StakingRegistration(address, signature)
    val value_1_in: Value =
      Value.defaultInstance.withTopl(
        Value.TOPL(
          quantity = BigInt(1),
          registration = Some(stakingRegistration)
        )
      )

    val value_1_out = Value.defaultInstance.withUpdateProposal(
      Value.UpdateProposal(label = "Proposal update 1", vrfPrecision = Some(1))
    )

    // do define if a topl should be burned or not when minting UpdateProposal
    val value_2_out = Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = None))

    val inputs = List(SpentTransactionOutput(txoAddress_1, attFull, value_1_in))
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs)

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(TransactionSyntaxError.InvalidUpdateProposal(Seq(value_1_out.getUpdateProposal)))
    )
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }

  /**
   * Output does not contains the Topl, registration is none
   */
  test("Invalid data-input case 4, minting a proposal updated Token") {

    val address = StakingAddress(ByteString.copyFrom(Array.fill(32)(0: Byte)))
    val stakingRegistration = StakingRegistration(address, signature)
    val value_1_in =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val updateProposal = Value.UpdateProposal(label = "Proposal update 1", vrfPrecision = Some(1))
    val value_1_out = Value.defaultInstance.withUpdateProposal(updateProposal)

    // do define if a topl should be burned or not when minting UpdateProposal
    val value_2_out =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = None))

    val inputs = List(SpentTransactionOutput(txoAddress_1, attFull, value_1_in))
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out)
    )

    val stms = List(UpdateProposalMintingStatement(updateProposal.computeId, txoAddress_1))

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, updateProposalMintingStatements = stms)

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(TransactionSyntaxError.InvalidUpdateProposal(Seq(value_1_out.getUpdateProposal)))
    )
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }

  /**
   * UpdateProposalMintingStatement repeated
   */
  test("Invalid data-input case 5 minting a proposal updated Token") {

    val address = StakingAddress(ByteString.copyFrom(Array.fill(32)(0: Byte)))
    val stakingRegistration = StakingRegistration(address, signature)
    val value_1_in =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val updateProposal = Value.UpdateProposal(label = "Proposal update 1", vrfPrecision = Some(1))
    val value_1_out = Value.defaultInstance.withUpdateProposal(updateProposal)

    // do define if a topl should be burned or not when minting UpdateProposal
    val value_2_out =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val inputs = List(SpentTransactionOutput(txoAddress_1, attFull, value_1_in))
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out)
    )

    val stms = List(
      UpdateProposalMintingStatement(updateProposal.computeId, txoAddress_1),
      UpdateProposalMintingStatement(updateProposal.computeId, txoAddress_1)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, updateProposalMintingStatements = stms)

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(TransactionSyntaxError.InvalidUpdateProposal(Seq(value_1_out.getUpdateProposal)))
    )
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }

  /**
   * Output updateProposal are duplicated
   */
  test("Invalid data-input case 6 minting a proposal updated Token") {

    val address = StakingAddress(ByteString.copyFrom(Array.fill(32)(0: Byte)))
    val stakingRegistration = StakingRegistration(address, signature)
    val value_1_in =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val updateProposal = Value.UpdateProposal(label = "Proposal update 1", vrfPrecision = Some(1))

    val value_1_out = Value.defaultInstance.withUpdateProposal(updateProposal)
    val value_2_out = Value.defaultInstance.withUpdateProposal(updateProposal)

    val value_3_out =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val inputs = List(SpentTransactionOutput(txoAddress_1, attFull, value_1_in))
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out),
      UnspentTransactionOutput(trivialLockAddress, value_3_out)
    )

    val stms = List(
      UpdateProposalMintingStatement(updateProposal.computeId, txoAddress_1)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, updateProposalMintingStatements = stms)

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(
        TransactionSyntaxError.InvalidUpdateProposal(Seq(value_1_out.getUpdateProposal, value_2_out.getUpdateProposal))
      )
    )
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }

  /**
   * Output Topl are duplicated, 1 != 2, here 2 rules are being catch
   */
  test("Invalid data-input case 7 minting a proposal updated Token") {

    val address = StakingAddress(ByteString.copyFrom(Array.fill(32)(0: Byte)))
    val stakingRegistration = StakingRegistration(address, signature)
    val value_1_in =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val updateProposal = Value.UpdateProposal(label = "Proposal update 1", vrfPrecision = Some(1))

    val value_1_out = Value.defaultInstance.withUpdateProposal(updateProposal)

    val value_2_out =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val value_3_out =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val inputs = List(SpentTransactionOutput(txoAddress_1, attFull, value_1_in))
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out),
      UnspentTransactionOutput(trivialLockAddress, value_3_out)
    )

    val stms = List(
      UpdateProposalMintingStatement(updateProposal.computeId, txoAddress_1)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, updateProposalMintingStatements = stms)

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(
        TransactionSyntaxError.InsufficientInputFunds(
          List(value_1_in.value),
          List(value_2_out.value, value_3_out.value)
        )
      )
    )

    val assertError_2 = result.exists(
      _.toList.contains(
        TransactionSyntaxError.InvalidUpdateProposal(Seq(value_1_out.getUpdateProposal))
      )
    )

    assertEquals(assertError, true)
    assertEquals(assertError_2, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 2)
  }

  /**
   * Output Topl are duplicated
   */
  test("Valid data-input case minting a proposal updated Token") {

    val address = StakingAddress(ByteString.copyFrom(Array.fill(32)(0: Byte)))
    val stakingRegistration = StakingRegistration(address, signature)
    val value_1_in =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(2), registration = Some(stakingRegistration)))

    val updateProposal = Value.UpdateProposal(label = "Proposal update 1", vrfPrecision = Some(1))

    val value_1_out = Value.defaultInstance.withUpdateProposal(updateProposal)

    val value_2_out =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val value_3_out =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val inputs = List(SpentTransactionOutput(txoAddress_1, attFull, value_1_in))
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out),
      UnspentTransactionOutput(trivialLockAddress, value_3_out)
    )

    val stms = List(
      UpdateProposalMintingStatement(updateProposal.computeId, txoAddress_1)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, updateProposalMintingStatements = stms)

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(
        TransactionSyntaxError.InsufficientInputFunds(
          List(value_1_in.value),
          List(value_2_out.value, value_3_out.value)
        )
      )
    )

    assertEquals(assertError, false)
    assertEquals(result.map(_.toList.size).getOrElse(0), 0)
  }

  test("Valid data-input case minting a proposal updated Token") {

    val address = StakingAddress(ByteString.copyFrom(Array.fill(32)(0: Byte)))
    val stakingRegistration = StakingRegistration(address, signature)
    val value_1_in =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val updateProposal = Value.UpdateProposal(label = "Proposal update 1", vrfPrecision = Some(1))
    val value_1_out = Value.defaultInstance.withUpdateProposal(updateProposal)

    // do define if a topl should be burned or not when minting UpdateProposal
    val value_2_out =
      Value.defaultInstance.withTopl(Value.TOPL(quantity = BigInt(1), registration = Some(stakingRegistration)))

    val inputs = List(SpentTransactionOutput(txoAddress_1, attFull, value_1_in))
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out)
    )

    val stms = List(UpdateProposalMintingStatement(updateProposal.computeId, txoAddress_1))

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, updateProposalMintingStatements = stms)

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(TransactionSyntaxError.InvalidUpdateProposal(Seq(value_1_out.getUpdateProposal)))
    )
    assertEquals(assertError, false)
    assertEquals(result.map(_.toList.size).getOrElse(0), 0)
  }

}
