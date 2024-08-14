package co.topl.brambl.display

import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.syntax.{AssetType, GroupType, LvlType, SeriesType, ToplType, ValueTypeIdentifier}
import co.topl.brambl.utils.Encoding

trait TypeIdentifierDisplayOps {

  implicit val typeIdentifierDisplay: DisplayOps[ValueTypeIdentifier] = {
    case LvlType              => "LVL"
    case ToplType(_)          => "TOPL"
    case GroupType(groupId)   => s"Group(${groupId.display})"
    case SeriesType(seriesId) => s"Series(${seriesId.display})"
    case AssetType(groupIdOrAlloy, seriesIdOrAlloy) =>
      s"Asset(${Encoding.encodeToHex(groupIdOrAlloy.toByteArray)}, ${Encoding.encodeToHex(seriesIdOrAlloy.toByteArray)})"
    case _ => "Unknown txo type"
  }
}
