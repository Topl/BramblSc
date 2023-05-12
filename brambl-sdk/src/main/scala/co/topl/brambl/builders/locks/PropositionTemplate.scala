package co.topl.brambl.builders.locks

import co.topl.brambl.builders.BuilderError
import quivr.models.{Data, Digest, Proposition, VerificationKey}
import PropositionTemplate.PropositionType
import cats.implicits.{
  catsSyntaxApplicativeId,
  catsSyntaxEitherId,
  catsSyntaxEitherObject,
  catsSyntaxFlatten,
  toFlatMapOps,
  toFunctorOps,
  toTraverseOps
}
import cats.{Applicative, Monad}
import co.topl.quivr.api.Proposer

trait PropositionTemplate[F[_]] {
  val propositionType: PropositionType
  def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Proposition]]
}

object PropositionTemplate {
  sealed abstract class PropositionType(val label: String)

  object types {
    case object Locked extends PropositionType("locked")
    case object Height extends PropositionType("height")
    case object Tick extends PropositionType("tick")
    case object Digest extends PropositionType("digest")
    case object Signature extends PropositionType("signature")
    case object And extends PropositionType("and")
    case object Or extends PropositionType("or")
    case object Not extends PropositionType("not")
    case object Threshold extends PropositionType("threshold")
  }

  case class UnableToBuildPropositionTemplate(message: String, cause: Throwable = null)
      extends BuilderError(message, cause)

  case class LockedTemplate[F[_]: Monad](data: Option[Data]) extends PropositionTemplate[F] {
    override val propositionType: PropositionType = types.Locked

    override def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Proposition]] =
      Proposer.LockedProposer[F].propose(data).map(Right(_))
  }

  case class HeightTemplate[F[_]: Monad](chain: String, min: Long, max: Long) extends PropositionTemplate[F] {
    override val propositionType: PropositionType = types.Height

    override def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Proposition]] =
      Proposer.heightProposer[F].propose((chain, min, max)).map(Right(_))
  }

  case class TickTemplate[F[_]: Monad](min: Long, max: Long) extends PropositionTemplate[F] {
    override val propositionType: PropositionType = types.Tick

    override def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Proposition]] =
      Proposer.tickProposer[F].propose((min, max)).map(Right(_))
  }

  case class DigestTemplate[F[_]: Monad](routine: String, digest: Digest) extends PropositionTemplate[F] {
    override val propositionType: PropositionType = types.Digest

    override def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Proposition]] =
      Proposer.digestProposer[F].propose((routine, digest)).map(Right(_))
  }

  case class SignatureTemplate[F[_]: Monad](routine: String, entityIdx: Int) extends PropositionTemplate[F] {
    override val propositionType: PropositionType = types.Signature

    override def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Proposition]] =
      if (entityIdx >= 0 && entityIdx < entityVks.length)
        Proposer.signatureProposer[F].propose((routine, entityVks(entityIdx))).map(Right(_))
      else
        Either
          .left[BuilderError, Proposition](
            UnableToBuildPropositionTemplate(
              s"Signature Proposition failed. Index: $entityIdx. Length of VKs: $entityVks"
            )
          )
          .pure[F]
  }

  case class AndTemplate[F[_]: Monad](leftTemplate: PropositionTemplate[F], rightTemplate: PropositionTemplate[F])
      extends PropositionTemplate[F] {
    override val propositionType: PropositionType = types.And

    override def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Proposition]] =
      Applicative[F]
        .map2(leftTemplate.build(entityVks), rightTemplate.build(entityVks)) {
          case (Right(leftProposition), Right(rightProposition)) =>
            Proposer.andProposer[F].propose((leftProposition, rightProposition)).map(_.asRight[BuilderError])
          case (Left(error), _) => error.asLeft[Proposition].pure[F]
          case (_, Left(error)) => error.asLeft[Proposition].pure[F]
        }
        .flatten
  }

  case class OrTemplate[F[_]: Monad](leftTemplate: PropositionTemplate[F], rightTemplate: PropositionTemplate[F])
      extends PropositionTemplate[F] {
    override val propositionType: PropositionType = types.Or

    override def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Proposition]] =
      Applicative[F]
        .map2(leftTemplate.build(entityVks), rightTemplate.build(entityVks)) {
          case (Right(leftProposition), Right(rightProposition)) =>
            Proposer.orProposer[F].propose((leftProposition, rightProposition)).map(_.asRight[BuilderError])
          case (Left(error), _) => error.asLeft[Proposition].pure[F]
          case (_, Left(error)) => error.asLeft[Proposition].pure[F]
        }
        .flatten
  }

  case class NotTemplate[F[_]: Monad](innerTemplate: PropositionTemplate[F]) extends PropositionTemplate[F] {
    override val propositionType: PropositionType = types.Not

    override def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Proposition]] =
      innerTemplate.build(entityVks).flatMap {
        case Right(proposition) => Proposer.notProposer[F].propose(proposition).map(_.asRight[BuilderError])
        case Left(error)        => error.asLeft[Proposition].pure[F]
      }
  }

  case class ThresholdTemplate[F[_]: Monad](innerTemplates: Seq[PropositionTemplate[F]], threshold: Int)
      extends PropositionTemplate[F] {
    override val propositionType: PropositionType = types.Threshold

    override def build(entityVks: List[VerificationKey]): F[Either[BuilderError, Proposition]] = {
      def recurseHelper(
        templates:   Seq[PropositionTemplate[F]],
        accumulator: Either[BuilderError, Seq[Proposition]]
      ): F[Either[BuilderError, Seq[Proposition]]] = accumulator match {
        case Left(err) => err.asLeft[Seq[Proposition]].pure[F] // Accumulator already encountered an error
        case Right(accProps) =>
          if (templates.isEmpty) accProps.asRight[BuilderError].pure[F] // All elements have been processed
          else
            templates.head.build(entityVks).flatMap { // < cannot do tailrec due to flattening of F
              case Left(err)      => err.asLeft[Seq[Proposition]].pure[F] // Current element encountered an error
              case Right(curProp) => recurseHelper(templates.tail, Right(accProps :+ curProp))
            }
      }
      recurseHelper(innerTemplates, Right(Seq.empty)).flatMap {
        case Left(err) => err.asLeft[Proposition].pure[F]
        case Right(props) =>
          Proposer.thresholdProposer[F].propose((props.toSet, threshold)).map(_.asRight[BuilderError])
      }
    }
  }
}
