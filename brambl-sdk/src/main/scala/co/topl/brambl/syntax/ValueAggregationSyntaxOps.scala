package co.topl.brambl.syntax

import co.topl.brambl.models.box.QuantityDescriptorType.{FRACTIONABLE, IMMUTABLE}
import co.topl.brambl.models.box.Value._

import scala.language.implicitConversions

trait ValueAggregationSyntaxOps {
  implicit def valueToAggregationSyntaxOps(v: Value): ValueToAggregationSyntaxOps = new ValueToAggregationSyntaxOps(v)
}

class ValueToAggregationSyntaxOps(val value: Value) extends AnyVal {
  /**
   * Adds two values together. That is, it adds the quantities of the values together
   * @note This is only valid for values of the same type
   * @note We cannot add IMMUTABLE or FRACTIONABLE values together
   * @param other The value to add to this value
   * @return The sum of the two values
   */
  def +(other: Value): Value =
    if(value.typeIdentifier == other.typeIdentifier) value.typeIdentifier.getQuantityDescriptor match {
      case Some(IMMUTABLE) || Some(FRACTIONABLE) => throw new Exception("Cannot add IMMUTABLE or FRACTIONABLE values")
      case _ => value.setQuantity(value.quantity + other.quantity)
    } else throw new Exception("Cannot add values of different types")
}
