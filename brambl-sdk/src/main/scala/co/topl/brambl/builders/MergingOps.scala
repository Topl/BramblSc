package co.topl.brambl.builders

import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.genus.services.Txo
import com.google.protobuf.ByteString
import com.google.protobuf.struct.Struct

object MergingOps {
  // TODO: strip ephermeral metadata and commitment??.. think about this
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

  def validMerge(values: Seq[Txo]): Boolean = ??? // check fungibility type
}
