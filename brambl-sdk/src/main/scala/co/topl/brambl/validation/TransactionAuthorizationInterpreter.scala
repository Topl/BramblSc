package co.topl.brambl.validation

import cats.Monad
import cats.implicits._
import co.topl.brambl.models.{Datum, Identifier}
import co.topl.brambl.models.transaction.{Attestation, IoTransaction}
import co.topl.quivr.api.Verifier
import co.topl.quivr.runtime.{DynamicContext, QuivrRuntimeError}
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
              input.attestation.value match {
                case Attestation.Value.Predicate(p) =>
                  predicateValidate(p.lock.challenges, p.lock.threshold, p.responses, context).map(r =>
                    r.map(_ => transaction)
                  )

                case Attestation.Value.Image32(p) =>
                  image32Validate(p.lock.leaves, p.lock.threshold, p.known, p.responses, context).map(r =>
                    r.map(_ => transaction)
                  )

                case Attestation.Value.Image64(p) =>
                  image64Validate(p.lock.leaves, p.lock.threshold, p.known, p.responses, context).map(r =>
                    r.map(_ => transaction)
                  )

                case Attestation.Value.Commitment32(p) =>
                  commitment32Validate(p.lock.root.get, p.lock.threshold, p.known, p.responses, context).map(r =>
                    r.map(_ => transaction)
                  )

                case Attestation.Value.Commitment64(p) =>
                  commitment64Validate(p.lock.root.get, p.lock.threshold, p.known, p.responses, context).map(r =>
                    r.map(_ => transaction)
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

      /***
       * Verifies that at least threshold number of proofs satisfy their associated propositions
       * @param propositions the propositions to be verified
       * @param proofs the proofs to be verified
       * @param threshold the threshold of proofs that must be satisfied
       * @param context the context in which the proofs are to be verified
       * @param verifier the verifier to be used to verify the proofs
       * @return
       */
      private def thresholdVerifier(
        propositions:      Seq[Proposition],
        proofs:            Seq[Proof],
        threshold:         Int,
        context:           DynamicContext[F, String, Datum]
      )(implicit verifier: Verifier[F, Datum]): F[Either[TransactionAuthorizationError, Boolean]] = {
        if (threshold === 0) true.asRight[TransactionAuthorizationError].pure[F]
        else if (threshold >= propositions.size)
          Either.left[TransactionAuthorizationError, Boolean](TransactionAuthorizationError.AuthorizationFailed()).pure[F]
        else if (proofs.isEmpty)
          Either.left[TransactionAuthorizationError, Boolean](TransactionAuthorizationError.AuthorizationFailed()).pure[F]
        // We assume a one-to-one pairing of sub-proposition to sub-proof with the assumption that some of the proofs
        // may be Proof.Value.Empty
        else if (proofs.size =!= propositions.size)
          Either.left[TransactionAuthorizationError, Boolean](TransactionAuthorizationError.AuthorizationFailed()).pure[F]
        else propositions.zip(proofs)
          .map(p => verifier.evaluate(p._1, p._2, context)) // Evaluate all the (proposition, proof) pairs
          .sequence
          .map(_.partitionMap(identity))
          .map(res => {
            // If at least threshold number of pairs are valid, authorization is successful
            if (res._2.count(identity) >= threshold)
              true.asRight[TransactionAuthorizationError]
            // If authorization fails, return the QuivrRuntimeErrors that were encountered
            else
              TransactionAuthorizationError.AuthorizationFailed(res._1.toList).asLeft[Boolean]
          })
      }
    }
}
