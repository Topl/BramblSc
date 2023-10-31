package co.topl.brambl.builders

import co.topl.brambl.models.box.QuantityDescriptorType.LIQUID
import co.topl.brambl.models.box.Value._
import co.topl.brambl.syntax.{
  bigIntAsInt128,
  int128AsBigInt,
  valueToQuantityDescriptorSyntaxOps,
  valueToQuantitySyntaxOps,
  valueToTypeIdentifierSyntaxOps,
  UnknownType
}

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
 * A trait containing operations for aggregating box values.
 */
trait AggregationOps {

  /**
   * Aggregates the quantities of a sequence of values if allowable.
   * If aggregation is not allowable, the values are returned unchanged.
   * Whether aggregation is allowable depends on the implementation.
   *
   * @param values The values to aggregate
   * @return The aggregated values
   */
  def aggregate(values: Seq[Value]): Seq[Value]

  /**
   * Aggregates the quantities of a sequence of values if allowable, and given an amount, partitions the result of
   * aggregation into 2 groups: the values that satisfy the amount and the change values that do not.
   *
   * If aggregation is not allowable, the values are returned unchanged and there will be no change.
   * If amount is not specified OR the quantities are not enough to satisfy the amount, there will be no change
   *
   * @param values The values to aggregate
   * @param amount The amount used to calculate change
   * @return The aggregated values and the change values
   */
  def aggregateWithChange(values: Seq[Value], amount: Option[BigInt]): (Seq[Value], Seq[Value])
}

/**
 * The default aggregation ops implementation.
 *
 * Values are allowed to be aggregated together under the following conditions:
 * - All are the same type
 * - The type is either GROUP, SERIES, liquid ASSET, or LVL
 *
 * Liquid ASSET denotes an ASSET with a quantity descriptor of LIQUID. Other quantity types (IMMUTABLE, FRACTIONABLE,
 * and ACCUMULATOR) are not allowed to be aggregated with this default implementation.
 */
object DefaultAggregationOps extends AggregationOps {

  /**
   * Aggregate 2 values into 1 if allowable. Throw an exception otherwise.
   */
  private def handleAggregation(value: Value, other: Value): Value =
    if (value.typeIdentifier == UnknownType)
      throw new Exception("Aggregation of UnknownType is not allowed")
    else if (value.typeIdentifier == other.typeIdentifier)
      if (value.getQuantityDescriptor.forall(_ == LIQUID))
        value.setQuantity(value.quantity + other.quantity)
      else throw new Exception("Aggregation of IMMUTABLE, FRACTIONABLE, or ACCUMULATOR assets is not allowed")
    else throw new Exception("Aggregation of different types is not allowed")

  override def aggregate(values: Seq[Value]): Seq[Value] = Try {
    values.reduce(handleAggregation)
  } match {
    case Success(v) => Seq(v)
    case Failure(_) => values
  }

  override def aggregateWithChange(values: Seq[Value], amount: Option[BigInt]): (Seq[Value], Seq[Value]) =
    amount match {
      case Some(transferQuantity) =>
        Try {
          values.reduce(handleAggregation)
        } match {
          case Success(v) =>
            if (v.quantity > transferQuantity)
              (Seq(v.setQuantity(transferQuantity)), Seq(v.setQuantity(v.quantity - transferQuantity)))
            else (Seq(v), Seq.empty[Value])
          case Failure(_) => (values, Seq.empty[Value])
        }
      case None => (aggregate(values), Seq.empty[Value])
    }
}
