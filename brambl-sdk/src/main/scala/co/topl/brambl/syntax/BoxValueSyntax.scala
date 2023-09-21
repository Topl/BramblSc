package co.topl.brambl.syntax

import co.topl.brambl.models.box.FungibilityType.{GROUP, GROUP_AND_SERIES, SERIES}
import co.topl.brambl.models.{GroupId, SeriesId}
import co.topl.brambl.models.box.Value._
import com.google.protobuf.ByteString
import quivr.models.Int128

import scala.language.implicitConversions

trait BoxValueSyntax {
  implicit def lvlAsBoxVal(lvl:  LVL): Value = Value.Lvl(lvl)
  implicit def groupAsBoxVal(g:  Group): Value = Value.Group(g)
  implicit def seriesAsBoxVal(s: Series): Value = Value.Series(s)
  implicit def assetAsBoxVal(a:  Asset): Value = Value.Asset(a)

  implicit def valueToQuantitySyntaxOps(v: Value): ValueToQuantitySyntaxOps = new ValueToQuantitySyntaxOps(v)

  implicit def valueToTypeIdentifierSyntaxOps(v: Value): ValueToTypeIdentifierSyntaxOps =
    new ValueToTypeIdentifierSyntaxOps(v)

}

class ValueToQuantitySyntaxOps(val value: Value) extends AnyVal {

  def quantity: Int128 = value match {
    case Value.Lvl(l)    => l.quantity
    case Value.Group(g)  => g.quantity
    case Value.Series(s) => s.quantity
    case Value.Asset(a)  => a.quantity
    case _               => throw new Exception("Invalid value type")
  }

  def setQuantity(quantity: Int128): Value = value match {
    case Value.Lvl(l)    => l.withQuantity(quantity)
    case Value.Group(g)  => g.withQuantity(quantity)
    case Value.Series(s) => s.withQuantity(quantity)
    case Value.Asset(a)  => a.withQuantity(quantity)
    case _               => throw new Exception("Invalid value type")
  }
}

class ValueToTypeIdentifierSyntaxOps(val value: Value) extends AnyVal {

  def typeIdentifier: ValueTypeIdentifier = value match {
    case Value.Lvl(_)    => LvlType
    case Value.Group(g)  => GroupType(g.groupId)
    case Value.Series(s) => SeriesType(s.seriesId)
    case Value.Asset(a) =>
      (a.fungibility, a.quantityDescriptor, a.groupId, a.seriesId, a.groupAlloy, a.seriesAlloy) match {
        case (GROUP_AND_SERIES, qd, Some(gId), Some(sId), _, _) => GroupAndSeriesFungible(gId, sId, qd)
        // If seriesAlloy is provided, the seriesId is ignored
        case (GROUP, qd, Some(gId), _, _, Some(sAlloy)) => GroupFungible(gId, sAlloy, qd)
        // If groupAlloy is provided, the groupId is ignored
        case (SERIES, qd, _, Some(sId), Some(gAlloy), _) => SeriesFungible(sId, gAlloy, qd)
        // If seriesAlloy is not provided, the seriesId is used to identify instead
        case (GROUP, qd, Some(gId), Some(sId), _, None) => GroupFungible(gId, sId.value, qd)
        // If groupAlloy is not provided, the groupId is used to identify instead
        case (SERIES, qd, Some(gId), Some(sId), None, _) => SeriesFungible(sId, gId.value, qd)
        case _                                           => throw new Exception("Invalid asset")
      }
    case _ => throw new Exception("Invalid value type")
  }
}

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

trait AssetType extends ValueTypeIdentifier

/**
 * A Group and Series fungible asset type, identified by a GroupId, a SeriesId, and an AggregateType.
 * The AggregateType is used to determine whether the asset can be aggregated with other assets of the same type.
 *
 * @param groupId The GroupId of the asset
 * @param series The SeriesId of the asset
 * @param aggregateType The AggregateType of the asset
 */
case class GroupAndSeriesFungible(groupId: GroupId, series: SeriesId, aggregateType: AggregateType) extends AssetType

/**
 * A Group fungible asset type, identified by a GroupId and a Series alloy. If the asset is not an alloy, the series
 * "alloy" is given by the seriesId.
 * The AggregateType is used to determine whether the asset can be aggregated with other assets of the same type.
 *
 * @param groupId  The GroupId of the asset
 * @param seriesAlloyOrId If the asset is an alloy, the Series alloy. Else the SeriesId of the asset
 * @param aggregateType The AggregateType of the asset
 */
case class GroupFungible(groupId: GroupId, seriesAlloyOrId: ByteString, aggregateType: AggregateType) extends AssetType

/**
 * A Series fungible asset type, identified by a SeriesId and a Group alloy. If the asset is not an alloy, the group
 * "alloy" is given by the groupId.
 * The AggregateType is used to determine whether the asset can be aggregated with other assets of the same type.
 *
 * @param series The SeriesId of the asset
 * @param groupAlloyOrId If the asset is an alloy, the Group alloy. Else the GroupId of the asset
 * @param aggregateType The AggregateType of the asset
 */
case class SeriesFungible(series: SeriesId, groupAlloyOrId: ByteString, aggregateType: AggregateType) extends AssetType
