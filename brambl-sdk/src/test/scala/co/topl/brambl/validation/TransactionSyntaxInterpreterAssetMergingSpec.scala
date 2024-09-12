package co.topl.brambl.validation

import cats.implicits._
import co.topl.brambl.MockHelpers
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances.lockImmutable
import co.topl.brambl.models.box.{Attestation, Challenge, Lock}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{LockAddress, LockId}
import co.topl.brambl.syntax._

/**
 * Test to coverage Asset Merging:
 * - all UTXOs (inputs and the new output) share the same (non group-and-series) fungibility type
 * - all UTXOs (inputs and the new output) share the same quantity descriptor type
 * - all UTXOs (inputs and the new output) have valid series/group alloy and IDs:
 *    - If group fungible, all UTXOs must share the same group ID. The new output must have a valid seriesAlloy and no seriedId
 *    - If series fungible, all UTXOs must share the same series ID. The new output must have a valid groupAlloy and no groupId
 * - the alloy fields are a valid merkle root of the input UTXO's values (in lexicographic order)
 * - input UTXOs are not reused in multiple merging statements
 * - input UTXOs are not present in the transaction outputs
 */
class TransactionSyntaxInterpreterAssetMergingSpec extends munit.FunSuite with MockHelpers {

  private val mockTransaction = IoTransaction.defaultInstance.withDatum(txDatum)

  // The following mock lock address and attestation are used for all the UTXOs in the tests
  private val mockLock = Lock.Predicate(List(MockHeightProposition).map(Challenge().withRevealed), 1)
  private val mockLockAddress =  LockAddress(0, 0, LockId(Lock().withPredicate(mockLock).sizedEvidence.digest.value))
  private val mockAttestation = Attestation().withPredicate(Attestation.Predicate(mockLock, List(MockHeightProof)))

  // use dummyTxoAddress for txoAddress (just update the index)

//  case class PartitionedMerge(inputs: Seq[Value], output: Value, asm: MergingStatement)

//  private def partitionMergingState

  /**
   * input UTXOs are not reused in multiple merging statements
   */
  test("Invalid case, an input UTXO is present in multiple merging statements") {
  }
  /**
   * input UTXOs are not present in the transaction outputs
   */
  test("Invalid case, an input UTXO to merge is also present in the transaction outputs") {
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
