package co.topl.brambl.builders.locks

import co.topl.brambl.builders.BuilderError
import co.topl.brambl.models.box.{Challenge, Lock}
import LockTemplate.LockType
import cats.Monad
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEitherId, toFlatMapOps}
import co.topl.brambl.builders.locks.PropositionTemplate.ThresholdTemplate
import quivr.models.{Proposition, VerificationKey}

trait LockTemplate[F[_]] {
  val lockType: LockType
  def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Lock]]
}

object LockTemplate {
  sealed abstract class LockType(val label: String)

  object types {
    case object Predicate extends LockType("predicate")
  }

  case class PredicateTemplate[F[_]: Monad](innerTemplates: Seq[PropositionTemplate[F]], threshold: Int)
      extends LockTemplate[F] {
    override val lockType: LockType = types.Predicate

    override def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Lock]] =
      // A Predicate Lock is very similar to a Threshold Proposition
      ThresholdTemplate[F](innerTemplates, threshold).build(entityVks).flatMap {
        case Left(error) => error.asLeft[Lock].pure[F]
        case Right(Proposition(Proposition.Value.Threshold(Proposition.Threshold(innerPropositions, _, _)), _)) =>
          Lock()
            .withPredicate(Lock.Predicate(innerPropositions.map(Challenge().withRevealed), threshold))
            .asRight[BuilderError]
            .pure[F]
      }
  }

}
