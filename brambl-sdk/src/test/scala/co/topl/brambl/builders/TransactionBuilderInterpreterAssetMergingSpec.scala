package co.topl.brambl.builders

import co.topl.brambl.models.box.FungibilityType.GROUP
import co.topl.brambl.models.box.QuantityDescriptorType.LIQUID
import co.topl.brambl.models.box.{AssetMergingStatement, Value}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.syntax.bigIntAsInt128
import com.google.protobuf.struct.{Struct, Value => StructValue}
import com.google.protobuf.struct.Value.Kind.StringValue

class TransactionBuilderInterpreterAssetMergingSpec extends MergingSpecBase {

  test("Invalid user params > a UTXO in UTXOs to merge does not exist in TXOs") {
    val testTx = buildAssertMergeTransaction
      .removeTxo(groupTxos.head.outputAddress)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("All UTXOs to merge must be accounted for in txos"))
        )
      )
    )
  }

  test("Invalid user params > UTXOs to merge are not compatible") {
    val txos = valuesToTxos(Seq(assetGroup, assetSeries))
    val testTx = buildAssertMergeTransaction
      .withUtxosToMerge(txos.map(_.outputAddress))
      .withTxos(txos ++ valuesToTxos(Seq(lvlValue, lvlValue, lvlValue), txos.length))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("Assets to merge must all share the same fungibility type"))
        )
      )
    )
  }

  test("Invalid user params > a TXO does not have a corresponding lock") {
    val testTx = buildAssertMergeTransaction
      .addTxo(valToTxo(lvlValue, trivialLockAddress.withLedger(3), dummyTxoAddress.withIndex(99)))
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("every lock in the txos must correspond to a lock in the lock map"))
        )
      )
    )
  }

  test("Invalid user params > a lock does not have a corresponding TXO") {
    val testTx = buildAssertMergeTransaction
      .addLock(trivialLockAddress.withLedger(3) -> trivialOutLock.getPredicate)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("every lock in the lock map must correspond to a lock in the txos"))
        )
      )
    )
  }

  test("Invalid user params > insufficient funds for fees") {
    val testTx = buildAssertMergeTransaction
      .updateFee(4L)
      .run
    assertEquals(
      testTx,
      Left(
        UserInputErrors(
          Seq(UserInputError("Not enough LVLs in input to satisfy fee"))
        )
      )
    )
  }

  test("Fee edge case (exact funds, no change)") {
    val testTx = buildAssertMergeTransaction
      .updateFee(3L)
      .run
    val merkleRoot = MergingOps.getAlloy(groupValues.map(_.getAsset))
    val expectedTx: IoTransaction = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMergingStatements(Seq(AssetMergingStatement(groupTxos.map(_.outputAddress), 0)))
      .withInputs(buildStxos(valuesToTxos(groupValues ++ Seq(lvlValue, lvlValue, lvlValue))))
      .withOutputs(
        buildRecipientUtxos(
          List(
            Value.defaultInstance.withAsset(
              Value.Asset(
                groupId = groupValues.head.getAsset.groupId,
                seriesId = None,
                groupAlloy = None,
                seriesAlloy = Some(merkleRoot),
                quantity = BigInt(groupValues.length),
                fungibility = GROUP,
                quantityDescriptor = LIQUID,
                ephemeralMetadata = None,
                commitment = None
              )
            )
          )
        )
      )
    assert(testTx.isRight && sortedTx(testTx.toOption.get).computeId == sortedTx(expectedTx).computeId)
  }

  test("Generic case") {
    val assetValues =
      groupValues :+ assetGroup.withAsset(assetGroup.getAsset.clearSeriesId.withSeriesAlloy(trivialByte32))
    val assetTxos = valuesToTxos(assetValues)
    val testTx = buildAssertMergeTransaction
      .withTxos(assetTxos ++ valuesToTxos(Seq(lvlValue, lvlValue, lvlValue), assetTxos.length))
      .withUtxosToMerge(assetTxos.map(_.outputAddress))
      .run
    val merkleRoot = MergingOps.getAlloy(assetValues.map(_.getAsset))
    val expectedTx: IoTransaction = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withMergingStatements(Seq(AssetMergingStatement(groupTxos.map(_.outputAddress), 1)))
      .withInputs(buildStxos(valuesToTxos(assetValues ++ Seq(lvlValue, lvlValue, lvlValue))))
      .withOutputs(
        buildChangeUtxos(List(lvlValue.withLvl(lvlValue.getLvl.withQuantity(BigInt(2)))))
        ++
        buildRecipientUtxos(
          List(
            Value.defaultInstance.withAsset(
              Value.Asset(
                groupId = assetValues.head.getAsset.groupId,
                seriesId = None,
                groupAlloy = None,
                seriesAlloy = Some(merkleRoot),
                quantity = BigInt(assetValues.length),
                fungibility = GROUP,
                quantityDescriptor = LIQUID,
                ephemeralMetadata = None,
                commitment = None
              )
            )
          )
        )
      )
    assert(testTx.isRight && sortedTx(testTx.toOption.get).computeId == sortedTx(expectedTx).computeId)
    // Ensure split utxo's commitment and metadata does not affect merkle root
    // Ensure split order does not matter
    val testSplit = assetValues
      .map(
        _.getAsset
          .withCommitment(trivialByte32)
          .withEphemeralMetadata(Struct(Map("test" -> StructValue(StringValue("test")))))
      )
      .reverse
    val testSplitMerkle = MergingOps.getAlloy(testSplit)
    assertEquals(testSplitMerkle, merkleRoot)
  }
}
