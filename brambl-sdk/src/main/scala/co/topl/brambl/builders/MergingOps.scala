package co.topl.brambl.builders

import cats.data.{Chain, Validated, ValidatedNec}
import cats.implicits.{catsSyntaxValidatedIdBinCompat0, toFoldableOps}
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.box.{FungibilityType, Value}
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.syntax.{AssetType, assetToAssetTypeSyntaxOps, valueToTypeIdentifierSyntaxOps}
import co.topl.genus.services.Txo
import com.google.protobuf.ByteString
import com.google.protobuf.struct.Struct

import scala.util.{Failure, Success, Try}

object MergingOps {
  // TODO: strip ephermeral metadata and commitment??..
  implicit def getPreimageBytes(utxo: UnspentTransactionOutput): Array[Byte] = utxo.value.immutable.toByteArray // includes quantity
  // Precondition: the values represent a valid merge
  def merge(values: Seq[Txo], mergedAssetLockAddress: LockAddress, ephemeralMetadata: Option[Struct], commitment: Option[ByteString]): UnspentTransactionOutput = {
    val quantity = ??? // fold the values
    UnspentTransactionOutput(
      mergedAssetLockAddress,
      Value.defaultInstance.withAsset(
        Value.Asset(
          groupId = ???, // based on fungibility
          seriesId = ???, // based on fungibility
          groupAlloy = ???, // based on fungibility AND merkle root - what about case where we are merging pre-existing alloys.. do we overwrite these fields?.. prob not bc the new merkle root will contain them
          seriesAlloy = ???, // based on fungibility AND merkle root
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
