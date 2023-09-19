package co.topl.brambl.validation

import cats.Id
import cats.implicits._
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.box.{AssetMintingStatement, Value}
import co.topl.brambl.models.transaction.{SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{Event, TransactionOutputAddress}
import co.topl.brambl.syntax._
import scala.language.implicitConversions

/**
 * Test to coverage this specific syntax validation:
 *  - Rule B - For all assets minting statement ams1, ams2, ...,  Should not contain repeated UTXOs
 */
class TransactionSyntaxInterpreterRuleBSpec extends munit.FunSuite with MockHelpers {

  private val txoAddress_1 = TransactionOutputAddress(1, 0, 0, dummyTxIdentifier)
  private val txoAddress_2 = TransactionOutputAddress(2, 0, 0, dummyTxIdentifier)

  /**
   * In this case there 2 validations that are failing;
   * DuplicateInput because input contains the same txoAddress
   * DuplicateInput because minting statements contains the same txoAddress
   */
  test("Invalid data-input case, input(0) + minted(1) == output(1), input and asset mining statements are duplicated") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_1)
    val value_1_in: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          seriesId = seriesPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1)
        )
      )

    // Note: duplicate sto address
    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_1, attFull, value_2_in)
    val output_1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_1_out)

    // Note: duplicate minting statements
    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_1,
      quantity = BigInt(1)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(
        TransactionSyntaxError.DuplicateInput(txoAddress_1)
      )
    )
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 2)

  }

  /**
   * In this case there 2 validations that are failing;
   * InsufficientInputFunds, because is not able to pass assetEqualFundsValidation
   * DuplicateInput because minting statements contains the same txoAddress
   */
  test("Invalid data-input case, input(0) + minted(1) == output(1), asset mining statements are duplicated") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)
    val value_1_in: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          seriesId = seriesPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1)
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_1_out)

    // Note: duplicate minting statements
    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_1,
      quantity = BigInt(1)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(
        TransactionSyntaxError.DuplicateInput(txoAddress_1)
      )
    )

    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 2)

  }

  /**
   * In this case there only 1 validation is failing, but contains 2 items;
   * DuplicateInput because minting statements contains the same txoAddress
   */
  test("Invalid data-input case, input(0) + minted(1) == output(1), asset mining statements are duplicated, case 2") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)
    val value_1_in: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          seriesId = seriesPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1)
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_1_out)

    // Note: duplicate minting statements, but in two statements mintingStatement_1 and mintingStatement_2
    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(1)
    )

    val mintingStatement_2 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(1)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1),
      mintingStatements = List(mintingStatement_1, mintingStatement_2)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(
        TransactionSyntaxError.DuplicateInput(txoAddress_1)
      )
    )

    val assertError_2 = result.exists(
      _.toList.contains(
        TransactionSyntaxError.DuplicateInput(txoAddress_2)
      )
    )

    assertEquals(assertError, true)
    assertEquals(assertError_2, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 3)

  }

}
