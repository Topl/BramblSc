package co.topl.brambl.display

import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.models.box.{AssetMintingStatement, Value}
import co.topl.brambl.utils.Encoding
import co.topl.brambl.syntax.int128AsBigInt

trait AssetDisplayOps {

  implicit val assetDisplay: DisplayOps[Value.Asset] = (asset: Value.Asset) =>
    Seq(
      "Asset",
      padLabel("GroupId") + asset.groupId.map(gId => gId.display).getOrElse("N/A"),
      padLabel("SeriesId") + asset.seriesId.map(sId => sId.display).getOrElse("N/A"),
      padLabel("Commitment") + asset.commitment
        .map(x => Encoding.encodeToHex(x.toByteArray()))
        .getOrElse("No commitment"),
      padLabel("Ephemeral-Metadata"),
      asset.ephemeralMetadata.map(meta => meta.display).getOrElse("No ephemeral metadata")
    ).mkString("\n")

  implicit val assetMintingStatementDisplay: DisplayOps[AssetMintingStatement] = (ams: AssetMintingStatement) =>
    Seq(
      padLabel("Group-Token-Utxo") + ams.groupTokenUtxo.display,
      padLabel("Series-Token-Utxo") + ams.seriesTokenUtxo.display,
      padLabel("Quantity") + (ams.quantity: BigInt).toString,
      padLabel("Permanent-Metadata"),
      ams.permanentMetadata.map(meta => meta.display).getOrElse("No permanent metadata")
    ).mkString("\n")
}
