package co.topl.brambl.builders

import cats.data.{Chain, Validated, ValidatedNec}
import cats.implicits.{catsSyntaxValidatedIdBinCompat0, toFoldableOps}
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.models.box.Value.Asset
import co.topl.brambl.models.box.{FungibilityType, Value}
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.models.{GroupId, LockAddress, SeriesId}
import co.topl.brambl.syntax.{AssetType, assetToAssetTypeSyntaxOps, bigIntAsInt128, int128AsBigInt, valueToTypeIdentifierSyntaxOps}
import co.topl.brambl.utils.Encoding.encodeToHex
import co.topl.crypto.accumulators.LeafData
import co.topl.crypto.accumulators.merkle.MerkleTree
import co.topl.crypto.hash.Sha
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.implicits.{digestDigest32, sha256Hash}
import co.topl.genus.services.Txo
import com.google.protobuf.ByteString
import com.google.protobuf.struct.Struct
import quivr.models.Int128

import scala.math.Ordering.Implicits._
import scala.util.{Failure, Success, Try}

object MergingOps {
  // We strip ephemeral metadata and commitment because when splitting the alloy in the future, these fields may be different in the outputs.
  implicit def getPreimageBytes(asset: Value.Asset): Array[Byte] = asset.clearEphemeralMetadata.clearCommitment.immutable.value.toByteArray
  // Get alloy preimages, sort, then construct merkle proof using Sha256.
  def getAlloy(values: Seq[Asset]): ByteString = ByteString.copyFrom(
    MerkleTree[Sha, Digest32](values.map(getPreimageBytes).sortBy(encodeToHex).map(LeafData)).rootHash.value
  )

  // Precondition: the values represent a valid merge
  def merge(values: Seq[Txo], mergedAssetLockAddress: LockAddress, ephemeralMetadata: Option[Struct], commitment: Option[ByteString]): UnspentTransactionOutput = {
    val quantity: Int128 = values.map(v => (v.transactionOutput.value.getAsset.quantity: BigInt)).sum
    val isGroupFungible = values.head.transactionOutput.value.getAsset.fungibility == FungibilityType.GROUP
    UnspentTransactionOutput(
      mergedAssetLockAddress,
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = Option.when(isGroupFungible)(GroupId(values.head.transactionOutput.value.getAsset.typeIdentifier.groupIdOrAlloy)),
          seriesId = Option.when(!isGroupFungible)(SeriesId(values.head.transactionOutput.value.getAsset.typeIdentifier.seriesIdOrAlloy)),
          groupAlloy = Option.when(!isGroupFungible)(getAlloy(values.map(_.transactionOutput.value.getAsset))),
          seriesAlloy = Option.when(isGroupFungible)(getAlloy(values.map(_.transactionOutput.value.getAsset))),
          quantity = quantity,
          fungibility = values.head.transactionOutput.value.getAsset.fungibility,
          quantityDescriptor = values.head.transactionOutput.value.getAsset.quantityDescriptor,
          ephemeralMetadata = ephemeralMetadata,
          commitment = commitment
        )
      )
    )
  }

  private def nonEmptyValidation(values: Seq[Txo]): ValidatedNec[String, Unit] =
    Validated.condNec(values.nonEmpty, (), "UTXOs to merge must not be empty")
  private def noDuplicatesValidation(values: Seq[Txo]): ValidatedNec[String, Unit] =
    Validated.condNec(values.distinctBy(_.outputAddress).length == values.length, (), "UTXOs to merge must not have duplicates")
  private def validIdentifiersValidation(values: Seq[Txo]): ValidatedNec[String, Unit] = Try {
    values.map(_.transactionOutput.value.value.typeIdentifier)
  } match {
    case Success(v) => if(v.forall({
      case AssetType(_, _) => true
      case _ => false
    })) ().validNec[String] else "UTXOs to merge must all be assets".invalidNec[Unit]
    case Failure(err) => err.getMessage.invalidNec[Unit]
  }
  private def distinctIdentifierValidation(values: Seq[Txo]): ValidatedNec[String, Unit] =
    Validated.condNec(values.map(_.transactionOutput.value.value.typeIdentifier).distinct.length == values.length, (), "UTXOs to merge must all be distinct (per type identifier)")
  private def sameFungibilityTypeValidation(values: Seq[Txo]): ValidatedNec[String, Unit] =
    Validated.condNec(values.forall(_.transactionOutput.value.getAsset.fungibility == values.head.transactionOutput.value.getAsset.fungibility), (), "Assets to merge must all share the same fungibility type")

  private def validFungibilityTypeValidation(values: Seq[Txo]): ValidatedNec[String, Unit] =
    (values.head.transactionOutput.value.getAsset.fungibility, values.head.transactionOutput.value.value.typeIdentifier) match {
    case (FungibilityType.GROUP_AND_SERIES, _) => "Assets to merge must not have Group_And_Series fungibility type".invalidNec[Unit]
    case (FungibilityType.SERIES, AssetType(_, seriesIdOrAlloy)) => Validated.condNec(
      values.tail.map(_.transactionOutput.value.getAsset.typeIdentifier.seriesIdOrAlloy).forall(_ == seriesIdOrAlloy), (), "Merging Series fungible assets must share a series ID"
    )
    case (FungibilityType.GROUP, AssetType(groupIdOrAlloy, _)) => Validated.condNec(
      values.tail.map(_.transactionOutput.value.getAsset.typeIdentifier.groupIdOrAlloy).forall(_ == groupIdOrAlloy), (), "Merging Group fungible assets must share a group ID"
    )
    case _ => "Merging Group or Series fungible assets do not have valid AssetType identifiers".invalidNec[Unit]
  }
  private def sameQuantityDescriptorValidation(values: Seq[Txo]): ValidatedNec[String, Unit] =
    Validated.condNec(values.nonEmpty, values.forall(_.transactionOutput.value.getAsset.quantityDescriptor == values.head.transactionOutput.value.getAsset.quantityDescriptor), "Merging assets must all share the same Quantity Descriptor Type")


  private val validators: Chain[Seq[Txo] => ValidatedNec[String, Unit]] = Chain(
    nonEmptyValidation, // seq not empty
    noDuplicatesValidation, // UTXO address does not repeat
    validIdentifiersValidation, // All TXOs have a valid identifier
    distinctIdentifierValidation, // IDs of all TXOs are distinct (combination of group/series ID/alloy)
    sameFungibilityTypeValidation, // All TXOs have same fungibility type
    validFungibilityTypeValidation, // not group_and_Series fungible, group or series fungibility have common ID
    sameQuantityDescriptorValidation, // ensure all TXOs have same quantity descriptor types
  )

  def validMerge(values: Seq[Txo]): ValidatedNec[String, Unit] = validators.foldMap(_ apply values)
}
