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
 *  - Validations only for minting, After projection  (only if for all inputs and outputs isMint == true)
 *    Case 3: Series
 *  - asset minted correspond to token supply in series policy
 */
class TransactionSyntaxInterpreterMintingCaseCSpec extends munit.FunSuite with MockHelpers {

  private val txoAddress_1 = TransactionOutputAddress(1, 0, 0, dummyTxIdentifier)
  private val txoAddress_2 = TransactionOutputAddress(2, 0, 0, dummyTxIdentifier)

  test("Valid data-input case 1, minting a Asset Token Unlimited") {
    val groupPolicy = Event.GroupPolicy(label = "policyG", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2, tokenSupply = None)

    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1), tokenSupply = None)
      )

    // case 1 token supply Unlimited
    // quantity could be any val 18, 1888, 999, and should be valid
    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1)
        )
      )

    val value_2_out: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_3_out: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)

    val output_1 = UnspentTransactionOutput(trivialLockAddress, value_1_out)
    val output_2 = UnspentTransactionOutput(trivialLockAddress, value_2_out)
    val output_3 = UnspentTransactionOutput(trivialLockAddress, value_3_out)

    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(1)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1, output_2, output_3),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    assertEquals(result.map(_.toList.size).getOrElse(0), 0)

  }

  test("Valid data-input case 1, minting a Asset Token Limited") {
    val groupPolicy = Event.GroupPolicy(label = "policyG", registrationUtxo = txoAddress_1)
    val seriesPolicy =
      Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2, tokenSupply = Some(10))

    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1), tokenSupply = Some(10))
      )

    // case 1 token supply Limited
    // only possible value is 10, 1*10.  if quantity is 2 = could be 10 or 20. 1*10, 2*10,
    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(10)
        )
      )

    // quantity should be equal value_1_in
    val value_2_out: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    // Here we burning the series, keep comment to understand the test
    // when we burn a series, means quantity =0, but this output is not produced
    //    val value_3_out: Value =
    //      Value.defaultInstance.withSeries(
    //        Value.Series(
    //          seriesId = seriesPolicy.computeId,
    //          quantity = BigInt(0) //  (assetQuantity(10) / tokenSupply(10)) - seriesQuantity(1) = 0
    //        )
    //      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1 = UnspentTransactionOutput(trivialLockAddress, value_1_out)
    val output_2 = UnspentTransactionOutput(trivialLockAddress, value_2_out)

    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(10)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1, output_2),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    assertEquals(result.map(_.toList.size).getOrElse(0), 0)

  }

  test("Valid data-input case 2, minting a Asset Token Limited") {
    val groupPolicy = Event.GroupPolicy(label = "policyG", registrationUtxo = txoAddress_1)
    val seriesPolicy =
      Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2, tokenSupply = Some(10))

    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(2), tokenSupply = Some(10))
      )

    // case 1 token supply Limited
    // only possible value is 10, 1*10.  if quantity is 2 = could be 10 or 20. 1*10, 2*10,
    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(20)
        )
      )

    // quantity should be equal value_1_in
    val value_2_out: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    // Here we burning the series, keep comment to understand the test
    // when we burn a 1 series, means quantity =0, but this output is not produced
    //    val value_3_out: Value =
    //      Value.defaultInstance.withSeries(
    //        Value.Series(
    //          seriesId = seriesPolicy.computeId,
    //          quantity = BigInt(0) //  (assetQuantity(10) / tokenSupply(10)) - seriesQuantity(1) = 0
    //        )
    //      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1 = UnspentTransactionOutput(trivialLockAddress, value_1_out)
    val output_2 = UnspentTransactionOutput(trivialLockAddress, value_2_out)

    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(20)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1, output_2),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    assertEquals(result.map(_.toList.size).getOrElse(0), 0)

  }

  test("Valid data-input case 3, minting a Asset Token Limited") {
    val groupPolicy = Event.GroupPolicy(label = "policyG", registrationUtxo = txoAddress_1)
    val seriesPolicy =
      Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2, tokenSupply = Some(10))

    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(2), tokenSupply = Some(10))
      )

    // case 1 token supply Limited
    // only possible value is 10, 1*10.  if quantity is 2 = could be 10 or 20. 1*10, 2*10,
    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(10)
        )
      )

    // quantity should be equal value_1_in
    val value_2_out: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    // Here we are not burning the series, we spend 1.
    val value_3_out: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          seriesId = seriesPolicy.computeId,
          quantity = BigInt(1),
          tokenSupply = Some(10)
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1 = UnspentTransactionOutput(trivialLockAddress, value_1_out)
    val output_2 = UnspentTransactionOutput(trivialLockAddress, value_2_out)
    val output_3 = UnspentTransactionOutput(trivialLockAddress, value_3_out)

    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(10)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1, output_2, output_3),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    assertEquals(result.map(_.toList.size).getOrElse(0), 0)

  }

  test("Valid data-input case 4, minting a Asset Token Limited") {
    val groupPolicy = Event.GroupPolicy(label = "policyG", registrationUtxo = txoAddress_1)
    val seriesPolicy =
      Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2, tokenSupply = Some(10))

    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(2), tokenSupply = Some(10))
      )

    // case 1 token supply Limited
    // only possible value is 10, 1*10.  if quantity is 2 = could be 10 or 20. 1*10, 2*10,
    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(20)
        )
      )

    // quantity should be equal value_1_in
    val value_2_out: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    // Here we burning the series, keep comment to understand the test, we burn 2
//    val value_3_out: Value =
//      Value.defaultInstance.withSeries(
//        Value.Series(
//          seriesId = seriesPolicy.computeId,
//          quantity = BigInt(0), // 2-2= 0
//          tokenSupply = Some(10)
//        )
//      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1 = UnspentTransactionOutput(trivialLockAddress, value_1_out)
    val output_2 = UnspentTransactionOutput(trivialLockAddress, value_2_out)

    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(20)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1, output_2),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    assertEquals(result.map(_.toList.size).getOrElse(0), 0)

  }

  /**
   * TODO I will keep test 5, but is wrong, because It should be handle in Transfer Series validation
   * - here we burn the series, in the output there should be no instances of Series
   */
  test("Valid data-input case 5, minting a Asset Token Limited") {
    val groupPolicy = Event.GroupPolicy(label = "policyG", registrationUtxo = txoAddress_1)
    val seriesPolicy =
      Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2, tokenSupply = Some(10))

    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(2), tokenSupply = Some(10))
      )

    // case 1 token supply Limited
    // only possible value is 10, 1*10.  if quantity is 2 = could be 10 or 20. 1*10, 2*10,
    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(20)
        )
      )

    // quantity should be equal value_1_in
    val value_2_out: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    // TODO, Here we are not burning the series, we spend 1. Burned Series, this output is wrong
    val value_3_out: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          seriesId = seriesPolicy.computeId,
          quantity = BigInt(1),
          tokenSupply = Some(10)
        )
      )

    // TODO, Here we are not burning the series, we spend 1. Burned Series, this output is wrong
    val value_4_out: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          seriesId = seriesPolicy.computeId,
          quantity = BigInt(1),
          tokenSupply = Some(10)
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1 = UnspentTransactionOutput(trivialLockAddress, value_1_out)
    val output_2 = UnspentTransactionOutput(trivialLockAddress, value_2_out)
    val output_3 = UnspentTransactionOutput(trivialLockAddress, value_3_out)
    val output_4 = UnspentTransactionOutput(trivialLockAddress, value_4_out)

    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(20)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1, output_2, output_3, output_4),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    assertEquals(result.map(_.toList.size).getOrElse(0), 0)

  }

  test("Invalid data-input case 1, minting a Asset Token Limited") {
    val groupPolicy = Event.GroupPolicy(label = "policyG", registrationUtxo = txoAddress_1)
    val seriesPolicy =
      Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2, tokenSupply = Some(10))

    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1), tokenSupply = Some(10))
      )

    // only possible value is 10, 1*10.  if quantity is 2 = could be 10 or 20. 1*10, 2*10,
    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(10)
        )
      )

    // quantity should be equal value_1_in
    val value_2_out: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1 = UnspentTransactionOutput(trivialLockAddress, value_1_out)
    val output_2 = UnspentTransactionOutput(trivialLockAddress, value_2_out)

    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(11) // Invalid data
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1, output_2),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(
        TransactionSyntaxError.InsufficientInputFunds(
          testTx.inputs.map(_.value.value).toList,
          testTx.outputs.map(_.value.value).toList
        )
      )
    )
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)

  }

  test("Invalid data-input case 2, minting a Asset Token Limited") {
    val groupPolicy = Event.GroupPolicy(label = "policyG", registrationUtxo = txoAddress_1)
    val seriesPolicy =
      Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2, tokenSupply = Some(10))

    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1), tokenSupply = Some(10))
      )

    // only possible value is 10, 1*10.  if quantity is 2 = could be 10 or 20. 1*10, 2*10,
    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(11) // Invalid Data
        )
      )

    // quantity should be equal value_1_in
    val value_2_out: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1 = UnspentTransactionOutput(trivialLockAddress, value_1_out)
    val output_2 = UnspentTransactionOutput(trivialLockAddress, value_2_out)

    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(10)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1, output_2),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(
        TransactionSyntaxError.InsufficientInputFunds(
          testTx.inputs.map(_.value.value).toList,
          testTx.outputs.map(_.value.value).toList
        )
      )
    )

    assertEquals(assertError, true)
    // 2 InsufficientInputFunds validation rules are catch
    assertEquals(result.map(_.toList.size).getOrElse(0), 2)

  }

  test("Invalid data-input case 3, minting a Asset Token Limited") {
    val groupPolicy = Event.GroupPolicy(label = "policyG", registrationUtxo = txoAddress_1)
    val seriesPolicy =
      Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2, tokenSupply = Some(3))

    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(2), tokenSupply = Some(3))
      )

    // possible value is 3, 6.
    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(2) // Invalid Data
        )
      )

    // quantity should be equal value_1_in
    val value_2_out: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1 = UnspentTransactionOutput(trivialLockAddress, value_1_out)
    val output_2 = UnspentTransactionOutput(trivialLockAddress, value_2_out)

    val mintingStatement_1 = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(2)
    )

    val testTx = txFull.copy(
      inputs = List(input_1, input_2),
      outputs = List(output_1, output_2),
      mintingStatements = List(mintingStatement_1)
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator.validate(testTx).swap

    val assertError = result.exists(
      _.toList.contains(
        TransactionSyntaxError.InsufficientInputFunds(
          testTx.inputs.map(_.value.value).toList,
          testTx.outputs.map(_.value.value).toList
        )
      )
    )

    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)

  }

}
