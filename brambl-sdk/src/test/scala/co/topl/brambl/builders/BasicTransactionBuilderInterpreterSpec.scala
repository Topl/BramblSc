package co.topl.brambl.builders

import co.topl.brambl.models.box.FungibilityType.GROUP_AND_SERIES
import co.topl.brambl.models.box.QuantityDescriptorType.LIQUID
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.syntax.{
  bigIntAsInt128,
  groupPolicyAsGroupPolicySyntaxOps,
  int128AsBigInt,
  ioTransactionAsTransactionSyntaxOps,
  seriesPolicyAsSeriesPolicySyntaxOps,
  valueToQuantitySyntaxOps
}

class BasicTransactionBuilderInterpreterSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildSimpleLvlTransaction > No change") {
    val testTx = txBuilder.buildSimpleLvlTransaction(
      List(valToTxo(lvlValue)),
      inPredicateLockFull,
      inPredicateLockFull,
      trivialLockAddress,
      1
    )
    assert(testTx.computeId == txFull.computeId)
  }

  test("buildSimpleLvlTransaction > With change") {
    val testTx = txBuilder.buildSimpleLvlTransaction(
      List(
        valToTxo(lvlValue.copy(lvlValue.value.setQuantity(lvlValue.value.quantity + 1)))
      ),
      inPredicateLockFull,
      trivialOutLock.getPredicate,
      trivialLockAddress,
      1
    )
    val expectedTx = txFull.copy(
      inputs = List(
        inputFull.copy(value = Value.defaultInstance.withLvl(Value.LVL(quantity + 1)))
      ),
      outputs = List(output, output)
    )
    assert(testTx.computeId == expectedTx.computeId)
  }

  test("lvlOutput (Predicate)") {
    assert(
      txBuilder.lvlOutput(trivialOutLock.getPredicate, quantity) == output
    )
  }

  test("lvlOutput (LockAddress)") {
    assert(txBuilder.lvlOutput(trivialLockAddress, quantity) == output)
  }

  test("lockAddress") {
    assert(txBuilder.lockAddress(inLockFull) == inLockFullAddress)
  }

  test("datum") {
    val testDatum = txBuilder.datum()
    // Testing fields individually since the timestamp is generated at runtime
    assert(testDatum.event.metadata == txDatum.event.metadata)
    assert(testDatum.event.schedule.min == txDatum.event.schedule.min)
    assert(testDatum.event.schedule.max == txDatum.event.schedule.max)
  }

  test("unprovenAttestation") {
    assert(txBuilder.unprovenAttestation(inPredicateLockFull) == attFull)
  }

  test("groupOutput") {
    assertEquals(
      txBuilder.groupOutput(trivialLockAddress, quantity, mockGroupPolicy.computeId, None),
      UnspentTransactionOutput(trivialLockAddress, groupValue)
    )
  }

  test("seriesOutput") {
    assertEquals(
      txBuilder.seriesOutput(trivialLockAddress, quantity, mockSeriesPolicy.computeId, None, GROUP_AND_SERIES, LIQUID),
      UnspentTransactionOutput(trivialLockAddress, seriesValue)
    )
  }

  test("assetOutput") {
    assertEquals(
      txBuilder.assetOutput(
        trivialLockAddress,
        quantity,
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        GROUP_AND_SERIES,
        LIQUID,
        None,
        None
      ),
      UnspentTransactionOutput(trivialLockAddress, assetGroupSeries)
    )
  }
}
