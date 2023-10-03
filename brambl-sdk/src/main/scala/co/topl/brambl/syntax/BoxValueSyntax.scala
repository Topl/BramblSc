package co.topl.brambl.syntax

import co.topl.brambl.models.box.FungibilityType.{GROUP, GROUP_AND_SERIES, SERIES}
import co.topl.brambl.models.box.QuantityDescriptorType
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
