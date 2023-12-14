package co.topl.brambl.display

import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.models.{Datum, SeriesId}
import co.topl.brambl.models.box.{FungibilityType, QuantityDescriptorType}
import co.topl.brambl.utils.Encoding
import co.topl.brambl.models.box.Value

trait SeriesDisplayOps {

  implicit val seriesIdDisplay: DisplayOps[SeriesId] = (id: SeriesId) => Encoding.encodeToHex(id.value.toByteArray())

  implicit val fungibilityDisplay: DisplayOps[FungibilityType] = {
    case FungibilityType.GROUP_AND_SERIES => "group-and-series"
    case FungibilityType.GROUP            => "group"
    case FungibilityType.SERIES           => "series"
    case _                                => throw new Exception("Unknown fungibility type") // this should not happen
  }

  implicit val quantityDescriptorDisplay: DisplayOps[QuantityDescriptorType] = {
    case QuantityDescriptorType.LIQUID       => "liquid"
    case QuantityDescriptorType.ACCUMULATOR  => "accumulator"
    case QuantityDescriptorType.FRACTIONABLE => "fractionable"
    case QuantityDescriptorType.IMMUTABLE    => "immutable"
    case _ => throw new Exception("Unknown quantity descriptor type") // should not happen
  }

  implicit val seriesPolicyDisplay: DisplayOps[Datum.SeriesPolicy] = (sp: Datum.SeriesPolicy) => s"""
Label: ${sp.event.label}
Regitration-Utxo: ${sp.event.registrationUtxo.display}
Fungibility: ${sp.event.fungibility.display}
Quantity-Descriptor: ${sp.event.quantityDescriptor.display}
Token-Supply: ${displayTokenSupply(sp.event.tokenSupply)}
Permanent-Metadata-Scheme:
${sp.event.permanentMetadataScheme.map(meta => meta.display).getOrElse("No permanent metadata")}
Ephemeral-Metadata-Scheme:
${sp.event.ephemeralMetadataScheme.map(meta => meta.display).getOrElse("No ephemeral metadata")}
    """

  implicit val seriesDisplay: DisplayOps[Value.Series] = (series: Value.Series) =>
    s"Series Constructor\n" +
    s"Id           : ${series.seriesId.display}\n" +
    s"Fungibility  : ${series.fungibility.display}\n" +
    s"Token-Supply : ${displayTokenSupply(series.tokenSupply)}\n" +
    s"Quant-Descr. : ${series.quantityDescriptor.display}"

  private def displayTokenSupply(tokenSupply: Option[Int]): String =
    tokenSupply.map(_.toString).getOrElse("UNLIMITED")
}
