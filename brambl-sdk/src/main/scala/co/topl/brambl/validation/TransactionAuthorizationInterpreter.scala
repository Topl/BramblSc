package co.topl.brambl.validation

import cats.Monad
import cats.implicits._
import co.topl.brambl.models.{Datum, Identifier}
import co.topl.brambl.models.transaction.{Attestation, IoTransaction}
import co.topl.quivr.api.Verifier
import co.topl.quivr.runtime.DynamicContext
import quivr.models.{Proof, Proposition}
import co.topl.brambl.validation.algebras.TransactionAuthorizationVerifier

/**
 * Validates that each Input within a Transaction is properly "authorized".  "Authorized" simply means "does the given
 * Proof satisfy the given Proposition?".
 */
object TransactionAuthorizationInterpreter {

  def make[F[_]: Monad]()(implicit verifier: Verifier[F, Datum]): TransactionAuthorizationVerifier[F] =
    new TransactionAuthorizationVerifier[F] {

      /**
       * Verifies each (Proposition, Proof) pair in the given Transaction
       */
      override def validate(context: DynamicContext[F, String, Datum])(
        transaction:                 IoTransaction
      ): F[Either[TransactionAuthorizationError, IoTransaction]] =
        transaction.inputs.zipWithIndex
          .foldLeft(Either.right[TransactionAuthorizationError, IoTransaction](transaction).pure[F]) {
            case (acc, (input, index)) =>
              input.attestation.get.value match {
                case Attestation.Value.Predicate(p) =>
                  predicateValidate(p.lock.get.challenges, p.lock.get.threshold, p.responses, context).map(r =>
                    r.map(_ => transaction)
                  )

                case Attestation.Value.Image32(p) =>
                  image32Validate(p.lock.get.leaves, p.lock.get.threshold, p.known, p.responses, context).map(r =>
                    r.map(_ => transaction)
                  )

                case Attestation.Value.Image64(p) =>
                  image64Validate(p.lock.get.leaves, p.lock.get.threshold, p.known, p.responses, context).map(r =>
                    r.map(_ => transaction)
                  )

                case Attestation.Value.Commitment32(p) =>
                  commitment32Validate(p.lock.get.root.get, p.lock.get.threshold, p.known, p.responses, context).map(
                    r => r.map(_ => transaction)
                  )

                case Attestation.Value.Commitment64(p) =>
                  commitment64Validate(p.lock.get.root.get, p.lock.get.threshold, p.known, p.responses, context).map(
                    r => r.map(_ => transaction)
                  )
              }
          }

      private def predicateValidate(
        challenges: Seq[Proposition],
        threshold:  Int,
        responses:  Seq[Proof],
        context:    DynamicContext[F, String, Datum]
      ): F[Either[TransactionAuthorizationError, Boolean]] =
        thresholdVerifier(challenges, responses, threshold, context)

      private def image32Validate(
        leaves:    Seq[Identifier.Lock32],
        threshold: Int,
        known:     Seq[Proposition],
        responses: Seq[Proof],
        context:   DynamicContext[F, String, Datum]
      ): F[Either[TransactionAuthorizationError, Boolean]] =
        // check that the known Propositions match the leaves?
        thresholdVerifier(known, responses, threshold, context)

      private def image64Validate(
        leaves:    Seq[Identifier.Lock64],
        threshold: Int,
        known:     Seq[Proposition],
        responses: Seq[Proof],
        context:   DynamicContext[F, String, Datum]
      ): F[Either[TransactionAuthorizationError, Boolean]] =
        thresholdVerifier(known, responses, threshold, context)

      // commitments need an additional proof of membership to be provided with the proposition
      private def commitment32Validate(
        root:      Identifier.AccumulatorRoot32,
        threshold: Int,
        known:     Seq[Proposition],
        responses: Seq[Proof],
        context:   DynamicContext[F, String, Datum]
      ): F[Either[TransactionAuthorizationError, Boolean]] =
        thresholdVerifier(known, responses, threshold, context)

      private def commitment64Validate(
        root:      Identifier.AccumulatorRoot64,
        threshold: Int,
        known:     Seq[Proposition],
        responses: Seq[Proof],
        context:   DynamicContext[F, String, Datum]
      ): F[Either[TransactionAuthorizationError, Boolean]] =
        thresholdVerifier(known, responses, threshold, context)

      private def thresholdVerifier(
        propositions:      Seq[Proposition],
        proofs:            Seq[Proof],
        threshold:         Int,
        context:           DynamicContext[F, String, Datum]
      )(implicit verifier: Verifier[F, Datum]): F[Either[TransactionAuthorizationError, Boolean]] = for {
        evalAuth <-
          if (threshold === 0) true.pure[F]
          else if (threshold >= propositions.size) false.pure[F]
          else if (proofs.isEmpty) false.pure[F]
          // We assume a one-to-one pairing of sub-proposition to sub-proof with the assumption that some of the proofs
          // may be Proofs.False
          else if (proofs.size =!= propositions.size) false.pure[F]
          else {
            propositions
              .zip(proofs)
              .foldLeftM(0L) {
                case (successCount, _) if successCount >= threshold =>
                  successCount.pure[F]
                case (successCount, (_, proof)) if proof.value.isEmpty =>
                  successCount.pure[F]
                case (successCount, (prop, proof)) =>
                  verifier.evaluate(prop, proof, context).map {
                    case Right(true) => successCount + 1
                    case _           => successCount
                  }
              }
              .map(_ >= threshold)
          }
        res <- Either.cond(evalAuth, evalAuth, TransactionAuthorizationError.AuthorizationFailed).pure[F]
      } yield res
    }
}
