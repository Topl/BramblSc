package co.topl.quivr.api

import cats._
import cats.data.OptionT
import cats.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.quivr._
import co.topl.quivr.runtime.QuivrRuntimeErrors.ValidationError.{
  EvaluationAuthorizationFailed,
  LockedPropositionIsUnsatisfiable,
  MessageAuthorizationFailed
}
import co.topl.quivr.runtime.{DynamicContext, QuivrRuntimeError}
import quivr.models._

import java.nio.charset.StandardCharsets

/**
 * A Verifier evaluates whether a given Proof satisfies a certain Proposition
 */
trait Verifier[F[_], Datum] {

  /**
   * Does the given `proof` satisfy the given `proposition` using the given `data`?
   */
  def evaluate(
    proposition: Proposition,
    proof:       Proof,
    context:     DynamicContext[F, String, Datum]
  ): F[Either[QuivrRuntimeError, Boolean]]
}

object Verifier {

  /**
   * @param tag     an identifier of the Operation
   * @param context the Dynamic evaluation context which should provide an API for retrieving the signable bytes
   * @return an array of bytes that is similar to a "signature" for the proof
   */
  private def evaluateBlake2b256Bind[F[_]: Monad, A](
    tag:         String,
    proof:       Proof,
    proofTxBind: TxBind,
    context:     DynamicContext[F, A, _]
  ): F[Either[QuivrRuntimeError, Boolean]] = for {
    sb <- context.signableBytes
    verifierTxBind = (new Blake2b256).hash(tag.getBytes(StandardCharsets.UTF_8) ++ sb.value.toByteArray)
    res = Either.cond(
      verifierTxBind sameElements proofTxBind.value.toByteArray,
      true,
      MessageAuthorizationFailed(proof)
    )
  } yield res

  /**
   * Collect the result of verification. Does the proof satisfy the proposition.
   * Both msgResult and evalResult need to indicate success
   *
   * @param proposition The proposition that the proof was verified against
   * @param proof The proof that was verified
   * @param msgResult Result of message validation. Success is denoted by Right(true)
   * @param evalResult Result of proposition and proof evaluation. Success is denoted by Right(_)
   * @return The result of verification. If successful, Right(true). Else Left(QuivrRuntimeError)
   */
  private def collectResult(proposition: Proposition, proof: Proof)(
    msgResult:  Either[QuivrRuntimeError, Boolean],
    evalResult: Either[QuivrRuntimeError, _]
  ): Either[QuivrRuntimeError, Boolean] = (msgResult, evalResult) match {
    case (Right(true), Right(_)) => Right[QuivrRuntimeError, Boolean](true)
    case _                       => Left[QuivrRuntimeError, Boolean](EvaluationAuthorizationFailed(proposition, proof))
  }

  trait Implicits {

    implicit class PropositionOps(proposition: Proposition) {

      def isSatisfiedBy[F[_], Datum](
        proof: Proof
      )(implicit
        context: DynamicContext[F, String, Datum],
        ev:      Verifier[F, Datum]
      ): F[Either[QuivrRuntimeError, Boolean]] =
        ev.evaluate(proposition, proof, context)
    }

    implicit class ProofOps(proof: Proof) {

      def satisfies[F[_], Datum](
        proposition: Proposition
      )(implicit
        ev:      Verifier[F, Datum],
        context: DynamicContext[F, String, Datum]
      ): F[Either[QuivrRuntimeError, Boolean]] =
        ev.evaluate(proposition, proof, context)
    }
  }

  object implicits extends Implicits

  trait Instances {

    private def lockedVerifier[F[_]: Monad](
      proposition: Proposition.Locked,
      proof:       Proof.Locked,
      context:     DynamicContext[F, String, _]
    ): F[Either[QuivrRuntimeError, Boolean]] =
      // should always fail, the Locked Proposition is unsatisfiable
      Either
        .left[QuivrRuntimeError, Boolean](LockedPropositionIsUnsatisfiable)
        .pure[F]

    private def digestVerifier[F[_]: Monad](
      proposition: Proposition.Digest,
      proof:       Proof.Digest,
      context:     DynamicContext[F, String, _]
    ): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withDigest(proposition).pure[F]
      wrappedProof       <- Proof().withDigest(proof).pure[F]
      msgResult          <- Verifier.evaluateBlake2b256Bind(Tokens.Digest, wrappedProof, proof.transactionBind, context)
      verification = DigestVerification(proposition.digest, proof.preimage)
      evalResult <- context.digestVerify(proposition.routine)(verification).value
      res = collectResult(wrappedProposition, wrappedProof)(msgResult, evalResult)
    } yield res

    private def signatureVerifier[F[_]: Monad](
      proposition: Proposition.DigitalSignature,
      proof:       Proof.DigitalSignature,
      context:     DynamicContext[F, String, _]
    ): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withDigitalSignature(proposition).pure[F]
      wrappedProof       <- Proof().withDigitalSignature(proof).pure[F]
      msgResult <- Verifier.evaluateBlake2b256Bind(
        Tokens.DigitalSignature,
        wrappedProof,
        proof.transactionBind,
        context
      )
      signedMessage <- context.signableBytes
      verification = SignatureVerification(
        proposition.verificationKey,
        proof.witness,
        Message(signedMessage.value)
      )
      evalResult <- context.signatureVerify(proposition.routine)(verification).value
      res = collectResult(wrappedProposition, wrappedProof)(msgResult, evalResult)
    } yield res

    private def heightVerifier[F[_]: Monad](
      proposition: Proposition.HeightRange,
      proof:       Proof.HeightRange,
      context:     DynamicContext[F, String, _]
    ): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withHeightRange(proposition).pure[F]
      wrappedProof       <- Proof().withHeightRange(proof).pure[F]
      msgResult <- Verifier.evaluateBlake2b256Bind(Tokens.HeightRange, wrappedProof, proof.transactionBind, context)
      chainHeight <- OptionT(context.heightOf(proposition.chain)).fold[Either[QuivrRuntimeError, Long]](
        Left(EvaluationAuthorizationFailed(wrappedProposition, wrappedProof))
      )(Right(_))
      evalResult = chainHeight match {
        case Right(h) =>
          if (proposition.min <= h && h <= proposition.max)
            Right(true)
          else Left(EvaluationAuthorizationFailed(wrappedProposition, wrappedProof))
        case Left(e) => Left(e)
      }
      res = collectResult(wrappedProposition, wrappedProof)(msgResult, evalResult)
    } yield res

    private def tickVerifier[F[_]: Monad](
      proposition: Proposition.TickRange,
      proof:       Proof.TickRange,
      context:     DynamicContext[F, String, _]
    ): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withTickRange(proposition).pure[F]
      wrappedProof       <- Proof().withTickRange(proof).pure[F]
      msgResult <- Verifier.evaluateBlake2b256Bind(Tokens.TickRange, wrappedProof, proof.transactionBind, context)
      evalResult <- context.currentTick.map(t =>
        if (proposition.min <= t && t <= proposition.max)
          Right(true)
        else Left(EvaluationAuthorizationFailed(wrappedProposition, wrappedProof))
      )
      res = collectResult(wrappedProposition, wrappedProof)(msgResult, evalResult)
    } yield res

    private def exactMatchVerifier[F[_]: Monad](
      proposition: Proposition.ExactMatch,
      proof:       Proof.ExactMatch,
      context:     DynamicContext[F, String, _]
    ): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withExactMatch(proposition).pure[F]
      wrappedProof       <- Proof().withExactMatch(proof).pure[F]
      msgResult  <- Verifier.evaluateBlake2b256Bind(Tokens.ExactMatch, wrappedProof, proof.transactionBind, context)
      evalResult <- context.exactMatch(proposition.location, proposition.compareTo.toByteArray).value
      res = collectResult(wrappedProposition, wrappedProof)(msgResult, evalResult)
    } yield res

    private def lessThanVerifier[F[_]: Monad](
      proposition: Proposition.LessThan,
      proof:       Proof.LessThan,
      context:     DynamicContext[F, String, _]
    ): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withLessThan(proposition).pure[F]
      wrappedProof       <- Proof().withLessThan(proof).pure[F]
      msgResult  <- Verifier.evaluateBlake2b256Bind(Tokens.LessThan, wrappedProof, proof.transactionBind, context)
      evalResult <- context.lessThan(proposition.location, BigInt(proposition.compareTo.value.toByteArray)).value
      res = collectResult(wrappedProposition, wrappedProof)(msgResult, evalResult)
    } yield res

    private def greaterThanVerifier[F[_]: Monad](
      proposition: Proposition.GreaterThan,
      proof:       Proof.GreaterThan,
      context:     DynamicContext[F, String, _]
    ): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withGreaterThan(proposition).pure[F]
      wrappedProof       <- Proof().withGreaterThan(proof).pure[F]
      msgResult  <- Verifier.evaluateBlake2b256Bind(Tokens.GreaterThan, wrappedProof, proof.transactionBind, context)
      evalResult <- context.greaterThan(proposition.location, BigInt(proposition.compareTo.value.toByteArray)).value
      res = collectResult(wrappedProposition, wrappedProof)(msgResult, evalResult)
    } yield res

    private def equalToVerifier[F[_]: Monad](
      proposition: Proposition.EqualTo,
      proof:       Proof.EqualTo,
      context:     DynamicContext[F, String, _]
    ): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withEqualTo(proposition).pure[F]
      wrappedProof       <- Proof().withEqualTo(proof).pure[F]
      msgResult  <- Verifier.evaluateBlake2b256Bind(Tokens.LessThan, wrappedProof, proof.transactionBind, context)
      evalResult <- context.equalTo(proposition.location, BigInt(proposition.compareTo.value.toByteArray)).value
      res = collectResult(wrappedProposition, wrappedProof)(msgResult, evalResult)
    } yield res

    private def thresholdVerifier[F[_]: Monad, Datum](
      proposition: Proposition.Threshold,
      proof:       Proof.Threshold,
      context:     DynamicContext[F, String, Datum]
    )(implicit verifier: Verifier[F, Datum]): F[Either[QuivrRuntimeError, Boolean]] =
      for {
        wrappedProposition <- Proposition().withThreshold(proposition).pure[F]
        wrappedProof       <- Proof().withThreshold(proof).pure[F]
        msgResult <- Verifier.evaluateBlake2b256Bind(Tokens.Threshold, wrappedProof, proof.transactionBind, context)
        evalResult <-
          if (proposition.threshold === 0) Right(true).pure[F]
          else if (proposition.threshold > proposition.challenges.size)
            Left(EvaluationAuthorizationFailed(wrappedProposition, wrappedProof)).pure[F]
          else if (proof.responses.isEmpty)
            Left(EvaluationAuthorizationFailed(wrappedProposition, wrappedProof)).pure[F]
          // We assume a one-to-one pairing of sub-proposition to sub-proof with the assumption that some of the proofs
          // may be Proofs.False
          else if (proof.responses.size =!= proposition.challenges.size)
            Left(EvaluationAuthorizationFailed(wrappedProposition, wrappedProof)).pure[F]
          else {
            proposition.challenges.toList
              .zip(proof.responses)
              .foldLeftM(0L) {
                case (successCount, _) if successCount >= proposition.threshold =>
                  successCount.pure[F]
                case (successCount, (_, proof)) if proof.value.isEmpty =>
                  successCount.pure[F]
                case (successCount, (prop: Proposition, proof)) =>
                  verifier.evaluate(prop, proof, context).map {
                    case Right(true) => (successCount + 1)
                    case _           => successCount
                  }
              }
              .map(_ >= proposition.threshold)
              .map(b => if (b) Right(true) else Left(EvaluationAuthorizationFailed(wrappedProposition, wrappedProof)))
          }
        res = collectResult(wrappedProposition, wrappedProof)(msgResult, evalResult)
      } yield res

    private def notVerifier[F[_]: Monad, Datum](
      proposition: Proposition.Not,
      proof:       Proof.Not,
      context:     DynamicContext[F, String, Datum]
    )(implicit verifier: Verifier[F, Datum]): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withNot(proposition).pure[F]
      wrappedProof       <- Proof().withNot(proof).pure[F]
      msgResult          <- Verifier.evaluateBlake2b256Bind(Tokens.Not, wrappedProof, proof.transactionBind, context)
      evalResult         <- verifier.evaluate(proposition.proposition, proof.proof, context)
      res = collectResult(wrappedProposition, wrappedProof)(msgResult, evalResult)
    } yield res match {
      case Right(true) => Left(EvaluationAuthorizationFailed(wrappedProposition, wrappedProof))
      case Left(EvaluationAuthorizationFailed(_, _)) => Right(true)
    }

    private def andVerifier[F[_]: Monad, Datum](
      proposition: Proposition.And,
      proof:       Proof.And,
      context:     DynamicContext[F, String, Datum]
    )(implicit verifier: Verifier[F, Datum]): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withAnd(proposition).pure[F]
      wrappedProof       <- Proof().withAnd(proof).pure[F]
      msgResult          <- Verifier.evaluateBlake2b256Bind(Tokens.And, wrappedProof, proof.transactionBind, context)
      aResult            <- verifier.evaluate(proposition.left, proof.left, context)
      bResult            <- verifier.evaluate(proposition.right, proof.right, context)
      res = (msgResult, aResult, bResult) match {

        case (Right(true), Right(_), Right(_)) => Right[QuivrRuntimeError, Boolean](true)
        case (_, aError, Right(_))             => aError
        case (_, Right(_), bError)             => bError
        case _ => Left[QuivrRuntimeError, Boolean](EvaluationAuthorizationFailed(wrappedProposition, wrappedProof))
      }
    } yield res

    private def orVerifier[F[_]: Monad, Datum](
      proposition: Proposition.Or,
      proof:       Proof.Or,
      context:     DynamicContext[F, String, Datum]
    )(implicit verifier: Verifier[F, Datum]): F[Either[QuivrRuntimeError, Boolean]] = for {
      wrappedProposition <- Proposition().withOr(proposition).pure[F]
      wrappedProof       <- Proof().withOr(proof).pure[F]
      msgResult          <- Verifier.evaluateBlake2b256Bind(Tokens.Or, wrappedProof, proof.transactionBind, context)
      aResult            <- verifier.evaluate(proposition.left, proof.left, context)
      bResult            <- verifier.evaluate(proposition.right, proof.right, context)
      res = (msgResult, aResult, bResult) match {
        case (Right(true), Right(_), _) => Right[QuivrRuntimeError, Boolean](true)
        case (Right(true), _, Right(_)) => Right[QuivrRuntimeError, Boolean](true)
        case (_, aError, Right(_))      => aError
        case (_, Right(_), bError)      => bError
        case _ => Left[QuivrRuntimeError, Boolean](EvaluationAuthorizationFailed(wrappedProposition, wrappedProof))
      }
    } yield res

    implicit def verifierInstance[F[_]: Monad, Datum]: Verifier[F, Datum] =
      new Verifier[F, Datum] {

        implicit private val v: Verifier[F, Datum] = this

        def evaluate(
          proposition: Proposition,
          proof:       Proof,
          context:     DynamicContext[F, String, Datum]
        ): F[Either[QuivrRuntimeError, Boolean]] =
          (proposition.value, proof.value) match {
            case (Proposition.Value.Locked(c), Proof.Value.Locked(r)) =>
              lockedVerifier(c, r, context)
            case (Proposition.Value.Digest(c), Proof.Value.Digest(r)) =>
              digestVerifier(c, r, context)
            case (Proposition.Value.DigitalSignature(c), Proof.Value.DigitalSignature(r)) =>
              signatureVerifier(c, r, context)
            case (Proposition.Value.HeightRange(c), Proof.Value.HeightRange(r)) =>
              heightVerifier(c, r, context)
            case (Proposition.Value.TickRange(c), Proof.Value.TickRange(r)) =>
              tickVerifier(c, r, context)
            case (Proposition.Value.ExactMatch(c), Proof.Value.ExactMatch(r)) =>
              exactMatchVerifier(c, r, context)
            case (Proposition.Value.LessThan(c), Proof.Value.LessThan(r)) =>
              lessThanVerifier(c, r, context)
            case (Proposition.Value.GreaterThan(c), Proof.Value.GreaterThan(r)) =>
              greaterThanVerifier(c, r, context)
            case (Proposition.Value.EqualTo(c), Proof.Value.EqualTo(r)) =>
              equalToVerifier(c, r, context)
            case (Proposition.Value.Threshold(c), Proof.Value.Threshold(r)) =>
              thresholdVerifier(c, r, context)
            case (Proposition.Value.Not(c), Proof.Value.Not(r)) =>
              notVerifier(c, r, context)
            case (Proposition.Value.And(c), Proof.Value.And(r)) =>
              andVerifier(c, r, context)
            case (Proposition.Value.Or(c), Proof.Value.Or(r)) =>
              orVerifier(c, r, context)
            // Proposition and Proof are not of the same type or either are Empty => Authorization fails
            case _ => Either.left[QuivrRuntimeError, Boolean](EvaluationAuthorizationFailed(proposition, proof)).pure[F]
          }
      }
  }

  object instances extends Instances
}
