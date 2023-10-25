package co.topl.brambl.syntax

import co.topl.brambl.models.{GroupId, SeriesId}
import co.topl.brambl.models.box.Value._
import com.google.protobuf.ByteString

import scala.language.implicitConversions

trait TokenTypeIdentifierSyntax {
  implicit def valueToTypeIdentifierSyntaxOps(v: Value): ValueToTypeIdentifierSyntaxOps =
    new ValueToTypeIdentifierSyntaxOps(v)
}

class ValueToTypeIdentifierSyntaxOps(val value: Value) extends AnyVal {

  def typeIdentifier: ValueTypeIdentifier = value match {
    case Value.Lvl(_)    => LvlType
    case Value.Group(g)  => GroupType(g.groupId)
    case Value.Series(s) => SeriesType(s.seriesId)
    case Value.Asset(a) =>
      (a.groupId, a.seriesId, a.groupAlloy, a.seriesAlloy) match {
        // If seriesAlloy is provided, the seriesId is ignored. In this case, groupAlloy should not exist
        case (Some(gId), _, None, Some(sAlloy)) => AssetType(gId.value, sAlloy)
        // If groupAlloy is provided, the groupId is ignored. In this case, seriesAlloy should not exist
        case (_, Some(sId), Some(gAlloy), None) => AssetType(gAlloy, sId.value)
        // if neither groupAlloy or seriesAlloy is provided, the groupId and seriesId are used to identify instead
        case (Some(gId), Some(sId), None, None) => AssetType(gId.value, sId.value)
        // invalid cases
        case (_, _, Some(_), Some(_)) => throw new Exception("Both groupAlloy and seriesAlloy cannot exist in an asset")
        case (_, _, None, None) => throw new Exception("Both groupId and seriesId must be provided for non-alloy assets")
        case (_, None, Some(_), _) => throw new Exception("seriesId must be provided when groupAlloy is used in an asset")
        case (None, _, _, Some(_)) => throw new Exception("groupId must be provided when seriesAlloy is used in an asset")
      }
    case _ => throw new Exception("Invalid value type")
  }
}

/**
 * Identifies the specific type of a token.
 */
trait ValueTypeIdentifier

/**
 * A LVL value type
 */
case object LvlType extends ValueTypeIdentifier

/**
 * A Group Constructor Token value type, identified by a GroupId
 *
 * @param groupId The GroupId of the Group Constructor Token
 */
case class GroupType(groupId: GroupId) extends ValueTypeIdentifier

/**
 * A Series Constructor Token value type, identified by a SeriesId
 * @param seriesId The SeriesId of the Series Constructor Token
 */
case class SeriesType(seriesId: SeriesId) extends ValueTypeIdentifier

/**
 * An Asset Token value type, identified by a Group Id (or Group Alloy) and a Series Id (or Series Alloy).
 *
 * If the asset is not an alloy (i.e, is not the result of a merge), then the GroupId and SeriesId of the asset are used.
 * Assets with a fungibility of GROUP_AND_SERIES can never be an alloy thus will always use GroupId and SeriesId.
 * If the asset is an alloy and it's fungibility is GROUP, then the GroupId and the Series Alloy of the asset are used.
 * If the asset is an alloy and it's fungibility is SERIES, then the Group Alloy and the SeriesId of the asset are used.
 *
 * @param groupIdOrAlloy The GroupId or Group Alloy of the asset
 * @param seriesIdOrAlloy The SeriesId or Series Alloy of the asset
 */
case class AssetType(groupIdOrAlloy: ByteString, seriesIdOrAlloy: ByteString) extends ValueTypeIdentifier
