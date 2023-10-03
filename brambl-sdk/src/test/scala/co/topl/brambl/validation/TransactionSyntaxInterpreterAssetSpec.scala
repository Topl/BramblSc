package co.topl.brambl.validation

import cats.Id
import cats.implicits._
import co.topl.brambl.MockHelpers
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

}
