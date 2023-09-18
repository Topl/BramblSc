package co.topl.brambl.syntax

import co.topl.brambl.models.box.FungibilityType.{GROUP, GROUP_AND_SERIES, SERIES}
import co.topl.brambl.models.{GroupId, SeriesId}
import co.topl.brambl.models.box.Value._
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
      (a.fungibility, a.groupId, a.seriesId) match {
        case (GROUP_AND_SERIES, Some(gId), Some(sId)) => GroupAndSeriesFungible(gId, sId)
        case (GROUP, Some(gId), _)                    => GroupFungible(gId)
        case (SERIES, _, Some(sId))                   => SeriesFungible(sId)
        case _                                        => throw new Exception("Invalid asset")
      }
    case _ => throw new Exception("Invalid value type")
  }
}

trait ValueTypeIdentifier

case object LvlType extends ValueTypeIdentifier

case class GroupType(groupId: GroupId) extends ValueTypeIdentifier

case class SeriesType(series: SeriesId) extends ValueTypeIdentifier

trait AssetType extends ValueTypeIdentifier

case class GroupAndSeriesFungible(groupId: GroupId, series: SeriesId) extends AssetType

case class GroupFungible(groupId: GroupId) extends AssetType

case class SeriesFungible(series: SeriesId) extends AssetType
