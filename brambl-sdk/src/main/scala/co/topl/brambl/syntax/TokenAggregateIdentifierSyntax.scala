package co.topl.brambl.syntax

import cats.implicits.catsSyntaxOptionId

import scala.language.implicitConversions

trait TokenAggregateIdentifierSyntax {

  implicit def typeToAggregateIdentifierSyntaxOps(
    typeIdentifier: ValueTypeIdentifier
  ): TypeToAggregateIdentifierSyntaxOps =
    new TypeToAggregateIdentifierSyntaxOps(typeIdentifier)

}

class TypeToAggregateIdentifierSyntaxOps(val typeIdentifier: ValueTypeIdentifier) extends AnyVal {

  def aggregateIdentifier: AggregateIdentifier = typeIdentifier match {
    case t: GroupAndSeriesFungible => AggregateIdentifier(t, (t.qdType: AggregateType).some)
    case t: GroupFungible          => AggregateIdentifier(t, (t.qdType: AggregateType).some)
    case t: SeriesFungible         => AggregateIdentifier(t, (t.qdType: AggregateType).some)
    case t                         => AggregateIdentifier(t)
  }
}

/**
 * Identifier used for aggregating tokens. To aggregate, tokens must be the same type and valid aggregate type.
 * I.e., Immutable or Fractionable tokens cannot be aggregated.
 */
case class AggregateIdentifier(typeIdentifier: ValueTypeIdentifier, aggregateType: Option[AggregateType] = None)
