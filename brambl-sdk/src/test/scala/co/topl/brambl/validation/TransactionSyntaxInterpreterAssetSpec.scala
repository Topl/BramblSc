package co.topl.brambl.validation

import cats.Id
import cats.implicits._
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.box.QuantityDescriptorType.{IMMUTABLE, LIQUID}
import co.topl.brambl.models.box.{AssetMintingStatement, FungibilityType, Value}
import co.topl.brambl.models.transaction.{SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{Event, TransactionOutputAddress}
import co.topl.brambl.syntax._

import scala.language.implicitConversions

/**
 * Test to coverage this specific syntax validation:
 * AssetEqualFundsValidation For each asset: input assets + minted assets == total asset output
 */
class TransactionSyntaxInterpreterAssetSpec extends munit.FunSuite with MockHelpers {

  private val txoAddress_1 = TransactionOutputAddress(1, 0, 0, dummyTxIdentifier)
  private val txoAddress_2 = TransactionOutputAddress(2, 0, 0, dummyTxIdentifier)

  /**
   * Reasons:
   * - input Assets = 0
   * - minted Assets = 0
   * - asset output = 1
   */
  test("Invalid data-input case 0+0 != 1") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val value_1: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          quantity = BigInt(1),
          groupId = Some(groupPolicy.computeId),
          fungibility = FungibilityType.GROUP
        )
      )
    val output_1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_1)
    val testTx = txFull.copy(outputs = List(output_1), mintingStatements = Seq.empty)

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

  /**
   * Reasons:
   * - input Assets = 1
   * - minted Assets = 0
   * - asset output = 1
   */
  test("Valid data-input case") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val value_1_in: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          quantity = BigInt(1),
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(mockSeriesPolicy.computeId),
          fungibility = FungibilityType.GROUP
        )
      )

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          quantity = BigInt(1),
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(mockSeriesPolicy.computeId),
          fungibility = FungibilityType.GROUP
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val output_1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_1_out)

    val testTx = txFull.copy(
      inputs = List(input_1),
      outputs = List(output_1),
      mintingStatements = Seq.empty
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
    assertEquals(assertError, false)

  }

  /**
   * Reasons:
   * - input Assets = 1
   * - minted Assets = 0
   * - asset output = 1
   * - The asset is not the same, first contains a groupId
   */
  test("Invalid data-input case not equals assets") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)
    val value_1_in: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP
        )
      )

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.SERIES
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val output_1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_1_out)

    val testTx = txFull.copy(
      inputs = List(input_1),
      outputs = List(output_1),
      mintingStatements = Seq.empty
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

  /**
   * Reasons:
   * - input Assets = 1, quantity = 1
   * - minted assets = 0
   * - output assets = 1, quantity = 2
   */
  test("Invalid data-input case: not equals quantity") {

    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)

    val value_1_in: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          quantity = BigInt(1),
          groupId = Some(groupPolicy.computeId),
          fungibility = FungibilityType.GROUP
        )
      )

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          quantity = BigInt(2),
          groupId = Some(groupPolicy.computeId),
          fungibility = FungibilityType.GROUP
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val output_1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_1_out)

    val testTx = txFull.copy(inputs = List(input_1), outputs = List(output_1), mintingStatements = Seq.empty)

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

  /**
   * Reasons:
   * - input Assets = 1
   * - minted Assets = 0
   * - asset output = 1
   */
  test("Invalid data-input case, equals assets,  2 + 0 != 1") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val value_1_in: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          quantity = BigInt(1),
          groupId = Some(groupPolicy.computeId),
          fungibility = FungibilityType.GROUP
        )
      )

    val value_2_in: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(quantity = BigInt(1), groupId = Some(groupPolicy.computeId), fungibility = FungibilityType.GROUP)
      )

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(groupId = Some(groupPolicy.computeId), fungibility = FungibilityType.GROUP, quantity = BigInt(1))
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val input_2 = SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    val output_1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_1_out)

    val testTx = txFull.copy(inputs = List(input_1, input_2), outputs = List(output_1), mintingStatements = Seq.empty)

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

  /**
   * Reasons:
   * - input Assets = 1
   * - minted Assets = 0
   * - asset output = 1
   */
  test("Valid data-input case, input(1) + minted(0) == output(1)") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val value_1_in: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(mockSeriesPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP
        )
      )

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(mockSeriesPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP
        )
      )

    val input_1 = SpentTransactionOutput(txoAddress_1, attFull, value_1_in)
    val output_1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, value_1_out)

    val testTx = txFull.copy(inputs = List(input_1), outputs = List(output_1), mintingStatements = List.empty)

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
    assertEquals(assertError, false)
  }

  /**
   * Reasons:
   * - input Assets = 0
   * - minted Assets = 1
   * - asset output = 1
   */
  test("Valid data-input case, input(0) + minted(1) == output(1)") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)
    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

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

    val inputs = List(
      SpentTransactionOutput(txoAddress_1, attFull, value_1_in),
      SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    )
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out),
      UnspentTransactionOutput(trivialLockAddress, value_3_out)
    )

    val mintingStatements =
      List(AssetMintingStatement(groupTokenUtxo = txoAddress_1, seriesTokenUtxo = txoAddress_2, quantity = BigInt(1)))

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, mintingStatements = mintingStatements)

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

    assertEquals(assertError, false)

  }

  /**
   * Reasons: Empty mintingStatements
   * - input Assets = 0
   * - minted Assets = 0
   * - asset output = 1
   */
  test("Invalid data-input case, input + minted == output") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)
    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

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

    val inputs = List(
      SpentTransactionOutput(txoAddress_1, attFull, value_1_in),
      SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    )

    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out),
      UnspentTransactionOutput(trivialLockAddress, value_3_out)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, mintingStatements = List.empty)

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

  /**
   * Reasons: This is expected to fail, but should works
   * - input Assets = 0
   * - minted Assets = 1
   * - asset output = 2
   */
  test("Invalid data-input case, input + minted == output") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)
    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(2) // check here
        )
      )

    val value_2_out: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_3_out: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

    val mintingStatements = List(
      AssetMintingStatement(
        groupTokenUtxo = txoAddress_1,
        seriesTokenUtxo = txoAddress_2,
        quantity = BigInt(1) // check here.
      )
    )

    val inputs = List(
      SpentTransactionOutput(txoAddress_1, attFull, value_1_in),
      SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    )

    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out),
      UnspentTransactionOutput(trivialLockAddress, value_3_out)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, mintingStatements = mintingStatements)

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

  /**
   * Reasons: This is expected to fail, but should works
   * - input Assets = 0
   * - minted Assets = 2
   * - asset output = 1
   */
  test("Invalid data-input case, input + minted == output") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)
    val value_1_in: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_2_in: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1) // check here
        )
      )

    val value_2_out: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_3_out: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

    val mintingStatements = List(
      AssetMintingStatement(
        groupTokenUtxo = txoAddress_1,
        seriesTokenUtxo = txoAddress_2,
        quantity = BigInt(2) // check here.
      )
    )

    val inputs = List(
      SpentTransactionOutput(txoAddress_1, attFull, value_1_in),
      SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    )
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out),
      UnspentTransactionOutput(trivialLockAddress, value_3_out)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, mintingStatements = mintingStatements)

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

  /**
   * Reasons:
   * - input Assets = 0
   * - minted Assets = 2
   * - asset output = 2
   */
  test("Valid data-input case, input + minted == output") {
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
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP_AND_SERIES
        )
      )

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1), // check here
          fungibility = FungibilityType.GROUP_AND_SERIES
        )
      )

    val value_2_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1), // check here
          fungibility = FungibilityType.GROUP_AND_SERIES
        )
      )

    val value_3_out: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val value_4_out: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

    val mintingStatements = List(
      AssetMintingStatement(
        groupTokenUtxo = txoAddress_1,
        seriesTokenUtxo = txoAddress_2,
        quantity = BigInt(2) // check here.
      )
    )

    val inputs = List(
      SpentTransactionOutput(txoAddress_1, attFull, value_1_in),
      SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    )

    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out),
      UnspentTransactionOutput(trivialLockAddress, value_3_out),
      UnspentTransactionOutput(trivialLockAddress, value_4_out)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, mintingStatements = mintingStatements)

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

    assertEquals(assertError, false)
    assertEquals(result.map(_.toList.size).getOrElse(0), 0)

  }

  /**
   * Reasons:
   * - input Assets = 0
   * - minted Assets = 2
   * - asset output = 2
   * - Fungibility Type is not the same
   */
  test("Invalid data-input case, input + minted == output") {
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
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP_AND_SERIES
        )
      )

    val value_1_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP // check here
        )
      )

    val value_2_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP_AND_SERIES // check here
        )
      )

    val value_3_out: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val value_4_out: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

    val mintingStatements = List(
      AssetMintingStatement(
        groupTokenUtxo = txoAddress_1,
        seriesTokenUtxo = txoAddress_2,
        quantity = BigInt(2) // check here.
      )
    )

    val inputs = List(
      SpentTransactionOutput(txoAddress_1, attFull, value_1_in),
      SpentTransactionOutput(txoAddress_2, attFull, value_2_in)
    )
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, value_1_out),
      UnspentTransactionOutput(trivialLockAddress, value_2_out),
      UnspentTransactionOutput(trivialLockAddress, value_3_out),
      UnspentTransactionOutput(trivialLockAddress, value_4_out)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, mintingStatements = mintingStatements)

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

  /**
   * Reasons:
   * - input Assets = 0
   * - minted Assets = 1
   * - asset output = 2
   *
   * An asset came from nowhere; different (groupId, seriesId) from AMS minted asset
   *
   * The first output's (groupId, seriesId) matches the AMS and satisfies the AMS quantity.
   * We still expected failure because the second output's (groupId, seriesId) is not from minting or inputs.
   */
  test("Invalid data-input case, unexpected asset output") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)
    val group_in: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val series_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          seriesId = seriesPolicy.computeId,
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP_AND_SERIES
        )
      )

    val minted_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP_AND_SERIES
        )
      )
    // SeriesId is different from the AMS so it is not the minted asset. There is also no matching input asset.
    val invalid_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.copy(fungibility = FungibilityType.GROUP).computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP // check here
        )
      )

    val group_out: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val series_out: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

    val mintingStatement = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(1)
    )

    val inputs = List(
      SpentTransactionOutput(txoAddress_1, attFull, group_in),
      SpentTransactionOutput(txoAddress_2, attFull, series_in)
    )
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, group_out),
      UnspentTransactionOutput(trivialLockAddress, series_out),
      UnspentTransactionOutput(trivialLockAddress, minted_out),
      UnspentTransactionOutput(trivialLockAddress, invalid_out)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, mintingStatements = List(mintingStatement))

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

  /**
   * Reasons:
   * - input Assets = 0
   * - minted Assets = 1
   * - asset output = 2
   *
   * Both output's (groupId, seriesId) match the AMS, but both have invalid fields.
   */
  test("Invalid data-input case, invalid fields in minted asset") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)
    val group_in: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val series_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          seriesId = seriesPolicy.computeId,
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP_AND_SERIES
        )
      )

    // Both the following outputs match the minted asset's group and series ID but have invalid fields
    val minted_out_invalidFungibility: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP
        )
      )
    val minted_out_invalidQuantity: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1),
          quantityDescriptor = IMMUTABLE
        )
      )

    val group_out: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val series_out: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

    val mintingStatement = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(1)
    )

    val inputs = List(
      SpentTransactionOutput(txoAddress_1, attFull, group_in),
      SpentTransactionOutput(txoAddress_2, attFull, series_in)
    )
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, group_out),
      UnspentTransactionOutput(trivialLockAddress, series_out),
      UnspentTransactionOutput(trivialLockAddress, minted_out_invalidFungibility),
      UnspentTransactionOutput(trivialLockAddress, minted_out_invalidQuantity)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, mintingStatements = List(mintingStatement))

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

  /**
   * Reasons:
   * - input Assets = 0
   * - minted Assets = 1
   * - asset output = 2
   *
   * Both output's (groupId, seriesId) match the AMS, but only the first has valid fields.
   */
  test("Invalid data-input case, invalid fields in one minted asset") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)
    val group_in: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          groupId = groupPolicy.computeId,
          quantity = BigInt(1)
        )
      )

    val series_in: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          seriesId = seriesPolicy.computeId,
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP_AND_SERIES
        )
      )

    // Both the following outputs match the minted asset's group and series ID but only the first has valid fields
    val minted_out: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP_AND_SERIES
        )
      )
    val minted_out_invalid: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1),
          quantityDescriptor = IMMUTABLE
        )
      )

    val group_out: Value =
      Value.defaultInstance.withGroup(Value.Group(groupId = groupPolicy.computeId, quantity = BigInt(1)))

    val series_out: Value =
      Value.defaultInstance.withSeries(Value.Series(seriesId = seriesPolicy.computeId, quantity = BigInt(1)))

    val mintingStatement = AssetMintingStatement(
      groupTokenUtxo = txoAddress_1,
      seriesTokenUtxo = txoAddress_2,
      quantity = BigInt(1)
    )

    val inputs = List(
      SpentTransactionOutput(txoAddress_1, attFull, group_in),
      SpentTransactionOutput(txoAddress_2, attFull, series_in)
    )
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, group_out),
      UnspentTransactionOutput(trivialLockAddress, series_out),
      UnspentTransactionOutput(trivialLockAddress, minted_out),
      UnspentTransactionOutput(trivialLockAddress, minted_out_invalid)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, mintingStatements = List(mintingStatement))

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

  /**
   * Reasons:
   * - input Assets = 2
   * - minted Assets = 0
   * - asset output = 2
   *
   * Asset outputs has invalid fields (compared to its input).
   */
  test("Invalid data-input case, invalid fields in transferred assets") {
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = txoAddress_1)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = txoAddress_2)

    val asset_in: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(2),
          fungibility = FungibilityType.GROUP_AND_SERIES,
          quantityDescriptor = LIQUID
        )
      )
    val asset_out_invalidFungibility: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP,
          quantityDescriptor = LIQUID
        )
      )
    val asset_out_invalidQuantityDescriptor: Value =
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Some(groupPolicy.computeId),
          seriesId = Some(seriesPolicy.computeId),
          quantity = BigInt(1),
          fungibility = FungibilityType.GROUP_AND_SERIES,
          quantityDescriptor = IMMUTABLE
        )
      )

    val inputs = List(
      SpentTransactionOutput(txoAddress_1, attFull, asset_in)
    )
    val outputs = List(
      UnspentTransactionOutput(trivialLockAddress, asset_out_invalidFungibility),
      UnspentTransactionOutput(trivialLockAddress, asset_out_invalidQuantityDescriptor)
    )

    val testTx = txFull.copy(inputs = inputs, outputs = outputs, mintingStatements = Seq.empty)

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
