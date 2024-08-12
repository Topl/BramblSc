package co.topl.brambl.builders

import cats.data.ValidatedNec
import co.topl.brambl.builders.MergingSpecBase.{BuildMergeStub, BuildValidMergeStub}
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.box.{QuantityDescriptorType, Value}
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.syntax.{groupPolicyAsGroupPolicySyntaxOps, seriesPolicyAsSeriesPolicySyntaxOps}
import co.topl.genus.services.Txo
import com.google.protobuf.ByteString
import com.google.protobuf.struct.Struct

trait MergingSpecBase extends munit.FunSuite with TransactionBuilderInterpreterSpecBase {
  def valuesToTxos(values: Seq[Value]): Seq[Txo] =
    values.zipWithIndex.map(x => (x._1, dummyTxoAddress.withIndex(x._2))).map(x => valToTxo(x._1, txAddr = x._2))

  def withUpdatedSeriesId(asset: Value): Value = asset.withAsset(
    asset.getAsset.withSeriesId(mockSeriesPolicyAlt.computeId)
  )
  def withUpdatedGroupId(asset: Value): Value = asset.withAsset(
    asset.getAsset.withGroupId(mockGroupPolicyAlt.computeId)
  )
  def withUpdatedQuantityDescriptor(asset: Value) = asset.withAsset(
    asset.getAsset.withQuantityDescriptor(QuantityDescriptorType.ACCUMULATOR)
  )

  val groupValues: Seq[Value] = Seq(assetGroup, withUpdatedSeriesId(assetGroup))
  val groupTxos: Seq[Txo] = valuesToTxos(groupValues)
  val seriesValues: Seq[Value] = Seq(assetSeries, withUpdatedGroupId(assetSeries))
  val seriesTxos: Seq[Txo] = valuesToTxos(seriesValues)

  def buildValidMerge: BuildValidMergeStub = BuildValidMergeStub(groupTxos)
  def buildMergeUtxo: BuildMergeStub = BuildMergeStub(groupTxos, RecipientAddr, None, None)

}

/**
 * Helpers for the Merging test cases
 */
object MergingSpecBase {

  case class BuildValidMergeStub(txos: Seq[Txo]) {

    def addTxo(txo: Txo): BuildValidMergeStub = this.copy(txos = txos :+ txo)
    def withTxos(newTxos: Seq[Txo]): BuildValidMergeStub = this.copy(txos = newTxos)

    def run: ValidatedNec[String, Unit] = MergingOps.validMerge(txos)
  }

  case class BuildMergeStub(values: Seq[Txo], mergedAssetLockAddress: LockAddress, ephemeralMetadata: Option[Struct], commitment: Option[ByteString]) {

    def addTxo(txo: Txo): BuildMergeStub = this.copy(values = values :+ txo)

    def withTxos(newTxos: Seq[Txo]): BuildMergeStub = this.copy(values = newTxos)
    def withEphemeralMetadata(newMeta: Option[Struct]): BuildMergeStub = this.copy(ephemeralMetadata = newMeta)
    def withCommitment(newCommitment: Option[ByteString]): BuildMergeStub = this.copy(commitment = newCommitment)

    def run: UnspentTransactionOutput = MergingOps.merge(values, mergedAssetLockAddress, ephemeralMetadata, commitment)
  }
}
