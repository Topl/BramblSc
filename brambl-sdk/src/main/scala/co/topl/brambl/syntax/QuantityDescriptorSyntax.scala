package co.topl.brambl.syntax

import co.topl.brambl.models.box.QuantityDescriptorType
import co.topl.brambl.models.box.QuantityDescriptorType.{FRACTIONABLE, IMMUTABLE}

import scala.language.implicitConversions

trait QuantityDescriptorSyntax {
  implicit def quantityDescriptorToAggregateType(qd: QuantityDescriptorType): AggregateType = new AggregateType(qd)
}

/**
 * Wraps a QuantityDescriptorType, but considers any non-mergeable descriptors (immutable or fractionable) as distinct.
 * For example AggregateType(Immutable) != AggregateType(Immutable)
 *
 * This will prevent aggregation of assets with these descriptors.
 *
 * @param qdType The original QuantityDescriptorType of the asset to wrap
 */
class AggregateType(val qdType: QuantityDescriptorType) {

  override def equals(obj: Any): Boolean =
    if (qdType == IMMUTABLE || qdType == FRACTIONABLE) false
    else
      obj match {
        case q: AggregateType => q.qdType == qdType
        case _                => false
      }

  override def toString: String = qdType.toString

  // It is not required to produce same hashcode if two objects are unequal (according to .equals)
  // However, it is required to produce same hashcode if two objects are equal (according to .equals)
  // If two instances of AggregateType are equal, then the underlying QuantityDescriptorType is guaranteed to be equal
  override def hashCode(): Int = qdType.hashCode()
}
