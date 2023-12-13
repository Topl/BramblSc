package co.topl.brambl.display

import co.topl.brambl.models.SeriesId
import co.topl.brambl.models.box.{FungibilityType, QuantityDescriptorType}
import co.topl.brambl.utils.Encoding

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
}
