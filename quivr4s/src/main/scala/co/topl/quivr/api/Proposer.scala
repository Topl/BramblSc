package co.topl.quivr.api

import cats.Applicative
import cats.implicits._
import com.google.protobuf.ByteString
import quivr.models.Data
import quivr.models.Digest
import quivr.models.Int128
import quivr.models.Proposition
import quivr.models.VerificationKey

// Proposers create Propositions from a tuple of arguments (or single argument) of type A.
trait Proposer[F[_], A] {
  def propose(args: A): F[Proposition]
}

// This represents the native apply (constructor) methods for the returned Proposition
object Proposer {

  def LockedProposer[F[_]: Applicative]: Proposer[F, Option[Data]] =
    (data: Option[Data]) => Proposition().withLocked(Proposition.Locked(data)).pure[F]

  def digestProposer[F[_]: Applicative]: Proposer[F, (String, Digest)] =
    (args: (String, Digest)) => Proposition().withDigest(Proposition.Digest(args._1, args._2)).pure[F]

  def signatureProposer[F[_]: Applicative]: Proposer[F, (String, VerificationKey)] =
    (args: (String, VerificationKey)) =>
      Proposition().withDigitalSignature(Proposition.DigitalSignature(args._1, args._2)).pure[F]

  def heightProposer[F[_]: Applicative]: Proposer[F, (String, Long, Long)] =
    (args: (String, Long, Long)) =>
      Proposition().withHeightRange(Proposition.HeightRange(args._1, args._2, args._3)).pure[F]

  def tickProposer[F[_]: Applicative]: Proposer[F, (Long, Long)] =
    (args: (Long, Long)) => Proposition().withTickRange(Proposition.TickRange(args._1, args._2)).pure[F]

  def exactMatchProposer[F[_]: Applicative]: Proposer[F, (String, Array[Byte])] =
    (args: (String, Array[Byte])) =>
      Proposition().withExactMatch(Proposition.ExactMatch(args._1, ByteString.copyFrom(args._2))).pure[F]

  def lessThanProposer[F[_]: Applicative]: Proposer[F, (String, Int128)] =
    (args: (String, Int128)) => Proposition().withLessThan(Proposition.LessThan(args._1, args._2)).pure[F]

  def greaterThan[F[_]: Applicative]: Proposer[F, (String, Int128)] =
    (args: (String, Int128)) => Proposition().withGreaterThan(Proposition.GreaterThan(args._1, args._2)).pure[F]

  def equalTo[F[_]: Applicative]: Proposer[F, (String, Int128)] =
    (args: (String, Int128)) => Proposition().withEqualTo(Proposition.EqualTo(args._1, args._2)).pure[F]

  def thresholdProposer[F[_]: Applicative]: Proposer[F, (Set[Proposition], Int)] =
    (args: (Set[Proposition], Int)) =>
      Proposition().withThreshold(Proposition.Threshold(args._1.toSeq, args._2)).pure[F]

  def notProposer[F[_]: Applicative]: Proposer[F, Proposition] =
    (args: Proposition) => Proposition().withNot(Proposition.Not(args)).pure[F]

  def andProposer[F[_]: Applicative]: Proposer[F, (Proposition, Proposition)] =
    (args: (Proposition, Proposition)) => Proposition().withAnd(Proposition.And(args._1, args._2)).pure[F]

  def orProposer[F[_]: Applicative]: Proposer[F, (Proposition, Proposition)] =
    (args: (Proposition, Proposition)) => Proposition().withOr(Proposition.Or(args._1, args._2)).pure[F]
}
