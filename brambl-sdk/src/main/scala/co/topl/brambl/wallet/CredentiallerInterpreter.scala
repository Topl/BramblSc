package co.topl.brambl.wallet

import cats.{Id, Monad}
import cats.implicits._
import co.topl.brambl.models.transaction.Attestation
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

object CredentiallerInterpreter {
  def make[F[_]: Monad](dataApi: DataApi): Credentialler[F] = new Credentialler[F] {
    override def prove(unprovenTx: IoTransaction): F[IoTransaction] = {
      val signable = unprovenTx.signable
      val provenInputs = unprovenTx.inputs.map(proveInput(_, signable))

      IoTransaction(provenInputs, unprovenTx.outputs, unprovenTx.datum).pure[F]
    }

    override def validate(tx: IoTransaction, ctx: Context[F]): F[List[ValidationError]] = {
      val combinedErrs = for {
        syntaxErrs <- EitherT(TransactionSyntaxInterpreter.make[F]().validate(tx)).swap
        authErr <- EitherT(TransactionAuthorizationInterpreter.make[F]().validate(ctx)(tx)).swap
      } yield syntaxErrs.toList :+ authErr
      combinedErrs.getOrElse(List.empty)
    }

    override def proveAndValidate(unprovenTx: IoTransaction, ctx: Context[F]): F[Either[List[ValidationError], IoTransaction]] =
      for {
        provenTx <- prove(unprovenTx)
        vErrs <- validate(provenTx, ctx)
      } yield if(vErrs.isEmpty) provenTx.asRight else vErrs.asLeft

    /**
     * Return a Proof (if possible) that will satisfy a Proposition and signable bytes
     *
     * It may not be possible to retrieve a proof if
     * - The proposition type is not yet supported (not one of Locked, Digest, Signature, Height and Tick)
     * - The secret data required for the proof is not available at idx
     *
     * @param msg         Signable bytes to bind to the proof
     * @param proposition Proposition in which the Proof should satisfy
     * @param idx         Indices for which the proof's secret data can be obtained from
     * @return The Proof (if possible)
     */
    private def getProof(msg: SignableBytes, proposition: Proposition, idx: Option[Indices]): Option[Proof] = {
      // TODO: Temporary until we have a way to map routines strings to the actual Routine
      val signingRoutines = Map(
        "ed25519" -> Ed25519Signature
      )
      proposition.value match {
        case _: Proposition.Value.Locked => Prover.lockedProver[Id].prove((), msg).some
        case _: Proposition.Value.Digest =>
          idx.flatMap(dataApi.getPreimage(_).map(Prover.digestProver[Id].prove(_, msg)))
        case Proposition.Value.DigitalSignature(p) =>
          signingRoutines
            .get(p.routine)
            .flatMap(r =>
              idx
                .flatMap(i => dataApi.getKeyPair(i, r))
                .flatMap(keyPair => keyPair.sk)
                .map(sk => Prover.signatureProver[Id].prove(r.sign(sk, msg), msg))
            )
        case _: Proposition.Value.HeightRange => Prover.heightProver[Id].prove((), msg).some
        case _: Proposition.Value.TickRange => Prover.tickProver[Id].prove((), msg).some
        case _ => None
      }
    }

    /**
     * *
     * Prove an input. That is, to prove all the propositions within the attestation
     *
     * If the wallet is unaware of the input's identifier, an error is returned
     *
     * @param input Input to prove. Once proven, the input can be spent
     *              Although the input is not yet spent, it is of type SpentTransactionOutput to denote its state
     *              after the transaction is accepted into the blockchain.
     * @param msg   signable bytes to bind to the proofs
     * @return The same input, but proven. If the input is unprovable, an error is returned.
     */
    private def proveInput(
                            input: SpentTransactionOutput,
                            msg: SignableBytes
                          ): SpentTransactionOutput = {
      val idx: Option[Indices] = dataApi.getIndicesByKnownIdentifier(input.knownIdentifier)
      val attestation: Attestation = input.attestation.value match {
          case Attestation.Value.Predicate(Attestation.Predicate(predLock, _, _)) =>
            Attestation().withPredicate(
              Attestation.Predicate(predLock, predLock.challenges.map(getProof(msg, _, idx).getOrElse(Proof())))
            )
          // TODO: We are not handling other types of Attestations at this moment in time
          case _ => ???
        }

      SpentTransactionOutput(input.knownIdentifier, attestation, input.value, input.datum, input.opts)
    }
  }
}