package co.topl.brambl.validation

import cats.Id
import cats.implicits._
import co.topl.brambl.MockHelpers
import co.topl.brambl.builders.MergingOps
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances.lockImmutable
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{GroupId, LockAddress, LockId, SeriesId}
import co.topl.brambl.syntax._
import co.topl.genus.services.Txo
import com.google.protobuf.ByteString
import quivr.models.Int128

/**
 * Test to coverage Asset Merging:
 * - all UTXOs (inputs and the new output) share the same (non group-and-series) fungibility type
 * - all UTXOs (inputs and the new output) share the same quantity descriptor type
 * - all UTXOs (inputs and the new output) have valid series/group alloy and IDs:
 *    - If group fungible, all UTXOs must share the same group ID. The new output must have a valid seriesAlloy and no seriedId
 *    - If series fungible, all UTXOs must share the same series ID. The new output must have a valid groupAlloy and no groupId
 * - the alloy fields are a valid merkle root of the input UTXO's values (in lexicographic order)
 * - merging statement contain 2 or more asset inputs that exist in the transaction
 * - merging statement's output's index does not refer to an asset output
 * - input UTXOs are not reused in multiple merging statements
 * - input UTXOs are not present in the transaction outputs
 */
class TransactionSyntaxInterpreterAssetMergingSpec extends munit.FunSuite with MockHelpers {

  val validator = TransactionSyntaxInterpreter.make[Id]()

  private val mockTransaction = IoTransaction.defaultInstance.withDatum(txDatum)

  // The following mock lock address and attestation are used for all the UTXOs in the tests
  private val mockLock = Lock.Predicate(List(MockHeightProposition).map(Challenge().withRevealed), 1)
  private val mockLockAddress =  LockAddress(0, 0, LockId(Lock().withPredicate(mockLock).sizedEvidence.digest.value))
  private val mockAttestation = Attestation().withPredicate(Attestation.Predicate(mockLock, List(MockHeightProof)))

  private val mockAsset = Value.Asset(quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)), fungibility = FungibilityType.GROUP)

  private val dummyBytes = ByteString.copyFrom(Array.fill(32)(0.toByte))

  private val groupTxos = for (i <- 1 to 3) yield Txo(
    transactionOutput = UnspentTransactionOutput(mockLockAddress, Value.defaultInstance.withAsset(
    mockAsset
      .withSeriesId(SeriesId(ByteString.copyFrom(Array.fill(32)(i.toByte))))
      .withGroupId(GroupId(dummyBytes))
  )), outputAddress = dummyTxoAddress.withIndex(i)
  )
  private val mergedGroup = MergingOps.merge(groupTxos, mockLockAddress, None, None)

  private val seriesTxos = for (i <- 4 to 6) yield Txo(
    transactionOutput = UnspentTransactionOutput(mockLockAddress, Value.defaultInstance.withAsset(
    mockAsset.withFungibility(FungibilityType.SERIES)
      .withSeriesId(SeriesId(dummyBytes))
      .withGroupId(GroupId(ByteString.copyFrom(Array.fill(32)(i.toByte))))
  )), outputAddress = dummyTxoAddress.withIndex(i)
  )
  private val mergedSeries = MergingOps.merge(seriesTxos, mockLockAddress, None, None)

  /**
   * Valid complex case:
   * - 2 asset merging statements, one for group fungible and one for series fungible (3 assets each)
   * - Transaction contains other UTXOs that are not used in the merging statements
   */
  test("Valid complex case") {
    val asmGroup = AssetMergingStatement(groupTxos.map(_.outputAddress), 1)
    val asmSeries = AssetMergingStatement(seriesTxos.map(_.outputAddress), 2)
    val inputs = (for (txo <- groupTxos ++ seriesTxos) yield SpentTransactionOutput(txo.outputAddress, mockAttestation, txo.transactionOutput.value)) :+ SpentTransactionOutput(dummyTxoAddress, mockAttestation, lvlValue)
    val outputs = Seq(UnspentTransactionOutput(mockLockAddress, lvlValue), mergedGroup, mergedSeries)
    val testTx = mockTransaction.withInputs(inputs).withOutputs(outputs).withMergingStatements(Seq(asmGroup, asmSeries))

    val result = validator.validate(testTx).toOption

    assert(result.isDefined) // If the result is defined, the validation passed
  }
  /**
   * input UTXOs are not reused in multiple merging statements
   */
  test("Invalid case, an input UTXO is present in multiple merging statements") {
    val asmGroup = AssetMergingStatement(groupTxos.map(_.outputAddress), 0)
    val asmGroupDup = AssetMergingStatement(groupTxos.map(_.outputAddress), 1)
    val inputs = for (txo <- groupTxos) yield SpentTransactionOutput(txo.outputAddress, mockAttestation, txo.transactionOutput.value)
    val outputs = Seq(mergedGroup, mergedGroup)
    val testTx = mockTransaction.withInputs(inputs).withOutputs(outputs).withMergingStatements(Seq(asmGroup, asmGroupDup))

    val result = validator.validate(testTx).swap
    val assertError = result.exists(_.toList.contains(TransactionSyntaxError.InvalidMergingStatements(Seq(asmGroup, asmGroupDup))))
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }
  /**
   * input UTXOs are not reused in multiple merging statements
   */
  test("Invalid case, an input UTXO is present multiple times in the same merging statement") {
    val asmGroup = AssetMergingStatement((groupTxos :+ groupTxos.head).map(_.outputAddress), 0)
    val inputs = for (txo <- groupTxos) yield SpentTransactionOutput(txo.outputAddress, mockAttestation, txo.transactionOutput.value)
    val outputs = Seq(MergingOps.merge(groupTxos :+ groupTxos.head, mockLockAddress, None, None))
    val testTx = mockTransaction.withInputs(inputs).withOutputs(outputs).withMergingStatements(Seq(asmGroup))

    val result = validator.validate(testTx).swap
    val assertError = result.exists(_.toList.contains(TransactionSyntaxError.InvalidMergingStatements(Seq(asmGroup))))
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }
  /**
   * merging statement contain 2 or more asset inputs that exist in the transaction
   */
  test("Invalid case, merging statement only contains 1 input") {
    val asmGroup = AssetMergingStatement(Seq(groupTxos.head.outputAddress), 0)
    val inputs = Seq(SpentTransactionOutput(groupTxos.head.outputAddress, mockAttestation, groupTxos.head.transactionOutput.value))
    val outputs = Seq(MergingOps.merge(Seq(groupTxos.head), mockLockAddress, None, None))
    val testTx = mockTransaction.withInputs(inputs).withOutputs(outputs).withMergingStatements(Seq(asmGroup))

    val result = validator.validate(testTx).swap
    val assertError = result.exists(_.toList.contains(TransactionSyntaxError.InvalidMergingStatements(Seq(asmGroup))))
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }
  /**
   * merging statement contain 2 or more asset inputs that exist in the transaction
   */
  test("Invalid case, merging statement contains a txo that does not exist in the transaction inputs") {
    val asmGroup = AssetMergingStatement(groupTxos.map(_.outputAddress), 0)
    val inputs = for (txo <- groupTxos.tail) yield SpentTransactionOutput(txo.outputAddress, mockAttestation, txo.transactionOutput.value)
    val outputs = Seq(MergingOps.merge(groupTxos.tail, mockLockAddress, None, None))
    val testTx = mockTransaction.withInputs(inputs).withOutputs(outputs).withMergingStatements(Seq(asmGroup))

    val result = validator.validate(testTx).swap
    val assertError = result.exists(_.toList.contains(TransactionSyntaxError.InvalidMergingStatements(Seq(asmGroup))))
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }
  /**
   * merging statement's output's index does not refer to an asset output
   */
  test("Invalid case, merging statement's output index is out of bounds") {
    val asmGroup = AssetMergingStatement(groupTxos.map(_.outputAddress), 1)
    val inputs = for (txo <- groupTxos) yield SpentTransactionOutput(txo.outputAddress, mockAttestation, txo.transactionOutput.value)
    val outputs = Seq(mergedGroup)
    val testTx = mockTransaction.withInputs(inputs).withOutputs(outputs).withMergingStatements(Seq(asmGroup))

    val result = validator.validate(testTx).swap
    val assertError = result.exists(_.toList.contains(TransactionSyntaxError.InvalidMergingStatements(Seq(asmGroup))))
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }
  /**
   * input UTXOs are not present in the transaction outputs
   */
  test("Invalid case, an input UTXO to merge is also present in the transaction outputs") {
    val asmGroup = AssetMergingStatement(groupTxos.map(_.outputAddress), 0)
    val inputs = for (txo <- groupTxos) yield SpentTransactionOutput(txo.outputAddress, mockAttestation, txo.transactionOutput.value)
    val outputs = Seq(mergedGroup, UnspentTransactionOutput(mockLockAddress, groupTxos.head.transactionOutput.value))
    val testTx = mockTransaction.withInputs(inputs).withOutputs(outputs).withMergingStatements(Seq(asmGroup))

    val result = validator.validate(testTx).swap
    val assertError = result.exists(_.toList.contains(TransactionSyntaxError.InvalidMergingStatements(Seq(asmGroup))))
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }
  /**
   * merging statement contain 2 or more asset inputs that exist in the transaction
   */
  test("Invalid case, merging statement's inputs are not assets") {
    val asmGroup = AssetMergingStatement(groupTxos.map(_.outputAddress), 0)
    val inputs = for (txo <- groupTxos) yield SpentTransactionOutput(txo.outputAddress, mockAttestation, lvlValue)
    val outputs = for (_ <- groupTxos.indices) yield UnspentTransactionOutput(mockLockAddress, lvlValue)
    val testTx = mockTransaction.withInputs(inputs).withOutputs(outputs).withMergingStatements(Seq(asmGroup))

    val result = validator.validate(testTx).swap
    val assertError = result.exists(_.toList.contains(TransactionSyntaxError.InvalidMergingStatements(Seq(asmGroup))))
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }
  /**
   * merging statement's output's index does not refer to an asset output
   */
  test("Invalid case, merging statement's output index refers to a non-asset") {
    val asmGroup = AssetMergingStatement(groupTxos.map(_.outputAddress), 0)
    val inputs = for (txo <- groupTxos) yield SpentTransactionOutput(txo.outputAddress, mockAttestation, txo.transactionOutput.value)
    val outputs = Seq(UnspentTransactionOutput(mockLockAddress, lvlValue))
    val testTx = mockTransaction.withInputs(inputs).withOutputs(outputs).withMergingStatements(Seq(asmGroup))

    val result = validator.validate(testTx).swap
    val assertError = result.exists(_.toList.contains(TransactionSyntaxError.InvalidMergingStatements(Seq(asmGroup))))
    assertEquals(assertError, true)
    assertEquals(result.map(_.toList.size).getOrElse(0), 1)
  }
  /**
   * all UTXOs (inputs and the new output) share the same (non group-and-series) fungibility type
   */
  test("Invalid case, UTXOs share GROUP_AND_SERIES fungibility type") {
  }
  /**
   * all UTXOs (inputs and the new output) share the same (non group-and-series) fungibility type
   */
  test("Invalid case, all inputs do not share the same fungibility type") {
  }
  /**
   * all UTXOs (inputs and the new output) share the same (non group-and-series) fungibility type
   */
  test("Invalid case, the output does not share the same fungibility type as the inputs") {
  }
  /**
   * all UTXOs (inputs and the new output) share the same quantity descriptor type
   */
  test("Invalid case, all inputs do not share the same fungibility type") {
  }
  /**
   * all UTXOs (inputs and the new output) share the same quantity descriptor type
   */
  test("Invalid case, the output does not share the same quantity descriptor type as the inputs") {
  }
  /**
   * If group fungible, all UTXOs must share the same group ID.
   */
  test("Invalid case, all inputs do not share the same group id") {
  }
  /**
   * If group fungible, all UTXOs must share the same group ID.
   */
  test("Invalid case, the output does not share the group id as the inputs") {
  }
  /**
   * If group fungible, The new output must have a valid seriesAlloy/seriesId.
   */
  test("Invalid case, the output does not have a series alloy") {
  }
  /**
   * If group fungible, The new output must have a valid seriesAlloy/seriesId.
   */
  test("Invalid case, the output has a seriesId defined") {
  }
  /**
   * If group fungible, The new output must have a valid seriesAlloy.
   */
  test("Invalid case, the output's series alloy is not a valid merkle root of the inputs") {
  }
  /**
   * If series fungible, all UTXOs must share the same series ID.
   */
  test("Invalid case, all inputs do not share the same series id") {
  }
  /**
   * If series fungible, all UTXOs must share the same series ID.
   */
  test("Invalid case, the output does not share the series id as the inputs") {
  }
  /**
   * If series fungible, The new output must have a valid groupAlloy/groupId .
   */
  test("Invalid case, the output does not have a group alloy") {
  }
  /**
   * If series fungible, The new output must have a valid groupAlloy/groupId .
   */
  test("Invalid case, the output has a group id defined") {
  }
  /**
   * If series fungible, The new output must have a valid groupAlloy.
   */
  test("Invalid case, the output's group alloy is not a valid merkle root of the inputs") {
  }
}
