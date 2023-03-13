package co.topl.brambl.wallet

import cats.Monad
import cats.implicits._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.routines.signatures.Ed25519Signature
import co.topl.brambl.validation.{TransactionAuthorizationInterpreter, TransactionSyntaxInterpreter, ValidationError}
import co.topl.brambl.Context
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.dataApi.DataApi
import co.topl.quivr.api.Prover
import co.topl.quivr.api.Verifier.instances.verifierInstance
import quivr.models.Proof
import quivr.models.Proposition
import quivr.models.SignableBytes
import co.topl.brambl.models.Indices
import cats.data.EitherT
import co.topl.brambl.models.box.Attestation

object CredentiallerInterpreter {

  def make[F[_]: Monad](dataApi: DataApi): Credentialler[F] = new Credentialler[F] {

    override def prove(unprovenTx: IoTransaction): F[IoTransaction] = {
      val signable = unprovenTx.signable
      unprovenTx.inputs
        .map(proveInput(_, signable))
        .sequence
        .map(IoTransaction(_, unprovenTx.outputs, unprovenTx.datum))
    }

    override def validate(tx: IoTransaction, ctx: Context[F]): F[List[ValidationError]] = {
      val combinedErrs = for {
        syntaxErrs <- EitherT(TransactionSyntaxInterpreter.make[F]().validate(tx)).swap
        authErr    <- EitherT(TransactionAuthorizationInterpreter.make[F]().validate(ctx)(tx)).swap
      } yield syntaxErrs.toList :+ authErr
      combinedErrs.getOrElse(List.empty)
    }

    override def proveAndValidate(
      unprovenTx: IoTransaction,
      ctx:        Context[F]
    ): F[Either[List[ValidationError], IoTransaction]] =
      for {
        provenTx <- prove(unprovenTx)
        vErrs    <- validate(provenTx, ctx)
      } yield if (vErrs.isEmpty) provenTx.asRight else vErrs.asLeft

    /**
     * Return a Proof that will satisfy a Proposition and signable bytes, if possible.
     * Otherwise return [[Proof.Value.Empty]]
     *
     * It may not be possible to retrieve a proof if
     * - The proposition type is not yet supported (not one of Locked, Digest, Signature, Height and Tick)
     * - The secret data required for the proof is not available at idx (or idx not provided)
     *
     * @param msg         Signable bytes to bind to the proof
     * @param proposition Proposition in which the Proof should satisfy
     * @param idx         Indices for which the proof's secret data can be obtained from
     * @return The Proof
     */
    private def getProof(msg: SignableBytes, proposition: Proposition, idx: Option[Indices]): F[Proof] = {
      // TODO: Temporary until we have a way to map routines strings to the actual Routine
      val signingRoutines = Map(
        Ed25519Signature.routine -> Ed25519Signature
      )
      proposition.value match {
        case _: Proposition.Value.Locked => Prover.lockedProver[F].prove((), msg)
        case _: Proposition.Value.Digest =>
          idx.flatMap(dataApi.getPreimage(_).map(Prover.digestProver[F].prove(_, msg))).getOrElse(Proof().pure[F])
        case Proposition.Value.DigitalSignature(p) =>
          signingRoutines
            .get(p.routine)
            .flatMap(r =>
              idx
                .flatMap(i => dataApi.getKeyPair(i, r))
                .map(keyPair => keyPair.sk)
                .map(sk => Prover.signatureProver[F].prove(r.sign(sk, msg), msg))
            )
            .getOrElse(Proof().pure[F])
        case _: Proposition.Value.HeightRange => Prover.heightProver[F].prove((), msg)
        case _: Proposition.Value.TickRange   => Prover.tickProver[F].prove((), msg)
        case _                                => Proof().pure[F]
      }
    }

    /**
     * Prove an input. That is, to prove all the propositions within the attestation.
     * If a proposition cannot be proven, it's proof will be [[Proof.Value.Empty]].
     *
     * @param input Input to prove. Once proven, the input can be spent
     *              Although the input is not yet spent, it is of type SpentTransactionOutput to denote its state
     *              after the transaction is accepted into the blockchain.
     * @param msg   signable bytes to bind to the proofs
     * @return The same input, but proven.
     */
    private def proveInput(
      input: SpentTransactionOutput,
      msg:   SignableBytes
    ): F[SpentTransactionOutput] = {
      val idx: Option[Indices] = dataApi.getIndicesByKnownIdentifier(input.address)
      val attestation: F[Attestation] = input.attestation.value match {
        case Attestation.Value.Predicate(Attestation.Predicate(predLock, _, _)) =>
          predLock.challenges
            // TODO: Fix .getRevealed
            .map(_.getRevealed)
            .map(getProof(msg, _, idx))
            .sequence
            .map(proofs => Attestation().withPredicate(Attestation.Predicate(predLock, proofs)))
        // TODO: We are not handling other types of Attestations at this moment in time
        case _ => ???
      }
      attestation.map(SpentTransactionOutput(input.address, _, input.value))
    }
  }
}
