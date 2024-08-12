package co.topl.brambl.builders

import co.topl.brambl.models.box.FungibilityType.{GROUP, SERIES}
import co.topl.brambl.models.box.QuantityDescriptorType.LIQUID
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.syntax.bigIntAsInt128
import com.google.protobuf.struct.Value.Kind.StringValue
import com.google.protobuf.struct.{Struct, Value => StrucValue}

class MergingOpsSpec extends MergingSpecBase {
  test("Valid Merge Compatibility > ") {
    val testValidMergeResult = buildValidMerge.run
    assert(testValidMergeResult.isValid, s"received: $testValidMergeResult")
  }
  test("Invalid Merge Compatibility > only 1 TXO provided") {
    val testValidMergeResult = buildValidMerge
      .withTxos(Seq(groupTxos.head))
      .run
    assert(testValidMergeResult.isInvalid &&
      testValidMergeResult.swap.toOption.get.head.contentEquals("There must be at least 2 UTXOs to merge"),
      s"received: $testValidMergeResult")
  }
  test("Invalid Merge Compatibility > Repeat UTXO address in TXOs") {
    val testValidMergeResult = buildValidMerge
      .addTxo(groupTxos.head)
      .run
    assert(testValidMergeResult.isInvalid &&
      testValidMergeResult.swap.toOption.get.head.contentEquals("UTXOs to merge must not have duplicates"),
      s"received: $testValidMergeResult")
  }
  test("Invalid Merge Compatibility > TXOs contain invalid identifier") {
    val testValidMergeResult = buildValidMerge
      .addTxo(valToTxo(groupValue.withAsset(groupValue.getAsset.clearGroupId), txAddr = dummyTxoAddress.withIndex(99)))
      .run
    assert(testValidMergeResult.isInvalid &&
      testValidMergeResult.swap.toOption.get.head.contentEquals("Both groupId and seriesId must be provided for non-alloy assets"),
      s"received: $testValidMergeResult")
  }
  test("Invalid Merge Compatibility > TXOs contain non-asset identifier") {
    val testValidMergeResult = buildValidMerge
      .addTxo(valToTxo(lvlValue, txAddr = dummyTxoAddress.withIndex(99)))
      .run
    assert(testValidMergeResult.isInvalid &&
      testValidMergeResult.swap.toOption.get.head.contentEquals("UTXOs to merge must all be assets"),
      s"received: $testValidMergeResult")
  }
  test("Invalid Merge Compatibility > Repeat TypeIdentifier in TXOs") {
    val testValidMergeResult = buildValidMerge
      .addTxo(valToTxo(assetGroup, txAddr = dummyTxoAddress.withIndex(99)))
      .run
    assert(testValidMergeResult.isInvalid &&
      testValidMergeResult.swap.toOption.get.head.contentEquals("UTXOs to merge must all be distinct (per type identifier)"),
      s"received: $testValidMergeResult")
  }
  test("Invalid Merge Compatibility > TXOs do not all share same fungibility type") {
    val testValidMergeResult = buildValidMerge
      .addTxo(valToTxo(assetSeries, txAddr = dummyTxoAddress.withIndex(99)))
      .run
    assert(testValidMergeResult.isInvalid &&
      testValidMergeResult.swap.toOption.get.head.contentEquals("Assets to merge must all share the same fungibility type"),
      s"received: $testValidMergeResult")
  }
  test("Invalid Merge Compatibility > TXOs contain Group_and_Series fungibility") {
    val testValidMergeResult = buildValidMerge
      .withTxos(valuesToTxos(Seq(assetGroupSeries, withUpdatedGroupId(assetGroupSeries))))
      .run
    assert(testValidMergeResult.isInvalid &&
      testValidMergeResult.swap.toOption.get.head.contentEquals("Assets to merge must not have Group_And_Series fungibility type"),
      s"received: $testValidMergeResult")
  }
  test("Invalid Merge Compatibility > TXOs with Series fungibility do not share SeriesId") {
    val testValidMergeResult = buildValidMerge
      .withTxos(valuesToTxos(withUpdatedSeriesId(seriesValues.head) +: seriesValues.tail))
      .run
    assert(testValidMergeResult.isInvalid &&
      testValidMergeResult.swap.toOption.get.head.contentEquals("Merging Series fungible assets must share a series ID"),
      s"received: $testValidMergeResult")
  }
  test("Invalid Merge Compatibility > TXOs with Group fungibility do not share GroupId") {
    val testValidMergeResult = buildValidMerge
      .withTxos(valuesToTxos(withUpdatedGroupId(groupValues.head) +: groupValues.tail))
      .run
    assert(testValidMergeResult.isInvalid &&
      testValidMergeResult.swap.toOption.get.head.contentEquals("Merging Group fungible assets must share a group ID"),
      s"received: $testValidMergeResult")
  }
  test("Invalid Merge Compatibility > TXOs do not all share same quantity descriptor type") {
    val testValidMergeResult = buildValidMerge
      .withTxos(valuesToTxos(withUpdatedQuantityDescriptor(groupValues.head) +: groupValues.tail))
      .run
    assert(testValidMergeResult.isInvalid &&
      testValidMergeResult.swap.toOption.get.head.contentEquals("Merging assets must all share the same Quantity Descriptor Type"),
      s"received: $testValidMergeResult")
  }
  test("Merge > Basic Group fungible") {
    val testMergedUtxo = buildMergeUtxo
      .withTxos(groupTxos)
      .run
    val merkleRoot = MergingOps.getAlloy(groupValues.map(_.getAsset))
    val expectedUtxo = UnspentTransactionOutput(
      RecipientAddr,
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = groupValues.head.getAsset.groupId,
          seriesId = None,
          groupAlloy = None,
          seriesAlloy = Some(merkleRoot),
          quantity = BigInt(2),
          fungibility = GROUP,
          quantityDescriptor = LIQUID,
          ephemeralMetadata = None,
          commitment = None
        )
      )
    )
    assertEquals(testMergedUtxo, expectedUtxo)
  }
  test("Merge > Basic Series fungible") {
    val testMergedUtxo = buildMergeUtxo
      .withTxos(seriesTxos)
      .run
    val merkleRoot = MergingOps.getAlloy(seriesValues.map(_.getAsset))
    val expectedUtxo = UnspentTransactionOutput(
      RecipientAddr,
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = None,
          seriesId = seriesValues.head.getAsset.seriesId,
          groupAlloy = Some(merkleRoot),
          seriesAlloy = None,
          quantity = BigInt(2),
          fungibility = SERIES,
          quantityDescriptor = LIQUID,
          ephemeralMetadata = None,
          commitment = None
        )
      )
    )
    assertEquals(testMergedUtxo, expectedUtxo)
  }
  test("Merge > Group fungible w/ an alloy") {
    val assetValues = groupValues :+ assetGroup.withAsset(assetGroup.getAsset.clearSeriesId.withSeriesAlloy(trivialByte32))
    val testMergedUtxo = buildMergeUtxo
      .withTxos(valuesToTxos(assetValues))
      .run
    val merkleRoot = MergingOps.getAlloy(assetValues.map(_.getAsset))
    val expectedUtxo = UnspentTransactionOutput(
      RecipientAddr,
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = assetValues.head.getAsset.groupId,
          seriesId = None,
          groupAlloy = None,
          seriesAlloy = Some(merkleRoot),
          quantity = BigInt(3),
          fungibility = GROUP,
          quantityDescriptor = LIQUID,
          ephemeralMetadata = None,
          commitment = None
        )
      )
    )
    assertEquals(testMergedUtxo, expectedUtxo)
  }
  test("Merge > Series fungible w/ an alloy") {
    val assetValues = seriesValues :+ assetSeries.withAsset(assetSeries.getAsset.clearGroupId.withGroupAlloy(trivialByte32))
    val testMergedUtxo = buildMergeUtxo
      .withTxos(valuesToTxos(assetValues))
      .run
    val merkleRoot = MergingOps.getAlloy(assetValues.map(_.getAsset))
    val expectedUtxo = UnspentTransactionOutput(
      RecipientAddr,
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = None,
          seriesId = assetValues.head.getAsset.seriesId,
          groupAlloy = Some(merkleRoot),
          seriesAlloy = None,
          quantity = BigInt(3),
          fungibility = SERIES,
          quantityDescriptor = LIQUID,
          ephemeralMetadata = None,
          commitment = None
        )
      )
    )
    assertEquals(testMergedUtxo, expectedUtxo)
  }
  test("Merge > Validate ephemeral metadata and commitment are added to merged UTXO") {
    val testMergedUtxo = buildMergeUtxo
      .withTxos(groupTxos)
      .withCommitment(Some(trivialByte32))
      .withEphemeralMetadata(Some(Struct(Map("test" -> StrucValue(StringValue("test"))))))
      .run
    val merkleRoot = MergingOps.getAlloy(groupValues.map(_.getAsset))
    val expectedUtxo = UnspentTransactionOutput(
      RecipientAddr,
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = groupValues.head.getAsset.groupId,
          seriesId = None,
          groupAlloy = None,
          seriesAlloy = Some(merkleRoot),
          quantity = BigInt(2),
          fungibility = GROUP,
          quantityDescriptor = LIQUID,
          ephemeralMetadata = Some(Struct(Map("test" -> StrucValue(StringValue("test"))))),
          commitment = Some(trivialByte32)
        )
      )
    )
    assertEquals(testMergedUtxo, expectedUtxo)
  }
  test("Merge > Validate ephemeral metadata and commitment does not affect merkle root") {
    val testMergedUtxo = buildMergeUtxo
      .withTxos(valuesToTxos(
        groupValues.head.withAsset(
          groupValues.head.getAsset.withCommitment(trivialByte32).withEphemeralMetadata(Struct(Map("test" -> StrucValue(StringValue("test")))))
        ) +: groupValues.tail))
      .run
    val expectedUtxo = buildMergeUtxo
      .withTxos(valuesToTxos(groupValues))
      .run
    assertEquals(testMergedUtxo, expectedUtxo)
  }
  test("Merge > Validate lexicographical order > changing orders does not affect merkle root") {
    val expectedUtxo = buildMergeUtxo
      .withTxos(groupTxos)
      .run
    val testMergedUtxo = buildMergeUtxo
      .withTxos(groupTxos.reverse)
      .run
    assertEquals(testMergedUtxo, expectedUtxo)
  }
}
