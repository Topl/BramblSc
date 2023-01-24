package co.topl.brambl.wallet

import cats.Id
import cats.implicits._
import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.Attestation
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.routines.signatures.Ed25519Signature
import co.topl.brambl.transaction.validators.ValidationError
import co.topl.brambl.transaction.validators.authorization.{
  TransactionAuthorizationError,
  TransactionAuthorizationInterpreter
}
import co.topl.brambl.transaction.validators.syntax.{TransactionSyntaxError, TransactionSyntaxErrors}
import co.topl.brambl.Context
import co.topl.brambl.typeclasses.ContainsSignable.instances.ioTransactionSignable
import co.topl.quivr.api.{Prover, Verifier}
import quivr.models.Proof
import quivr.models.Proposition
import quivr.models.SignableBytes
import co.topl.brambl.models.Indices

object MockCredentialler extends Credentials {

  /**
   * Return a Proof (if possible) that will satisfy a Proposition and signable bytes
   *
   * It may not be possible to retrieve a proof if
   * - The proposition type is not yet supported (not one of Locked, Digest, Signature, Height and Tick)
   * - The secret data required for the proof is not available at idx
   *
   * @param msg Signable bytes to bind to the proof
   * @param proposition Proposition in which the Proof should satisfy
   * @param idx Indices for which the proof's secret data can be obtained from
   * @return The Proof (if possible)
   */
  private def getProof(msg: SignableBytes, proposition: Proposition, idx: Option[Indices]): Option[Proof] = {
    // Temporary until we have a way to map routines strings to the actual Routine
    val signingRoutines = Map(
      "ed25519" -> Ed25519Signature
    )
    proposition.value match {
      case _: Proposition.Value.Locked => Prover.lockedProver[Id].prove((), msg).some
      case _: Proposition.Value.Digest =>
        idx.flatMap(MockStorage.getPreimage(_).map(Prover.digestProver[Id].prove(_, msg)))
      case Proposition.Value.DigitalSignature(p) =>
        signingRoutines
          .get(p.routine)
          .flatMap(r =>
            idx
              .flatMap(i => MockStorage.getKeyPair(i, r))
              .flatMap(keyPair => keyPair.sk)
              .map(sk => Prover.signatureProver[Id].prove(r.sign(sk, msg), msg))
          )
      case _: Proposition.Value.HeightRange => Prover.heightProver[Id].prove((), msg).some
      case _: Proposition.Value.TickRange   => Prover.tickProver[Id].prove((), msg).some
      case _                                => None
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
   * @param msg signable bytes to bind to the proofs
   * @return The same input, but proven. If the input is unprovable, an error is returned.
   */
  private def proveInput(
    input: SpentTransactionOutput,
    msg:   SignableBytes
  ): Either[TransactionSyntaxError, SpentTransactionOutput] = {
    val idx: Option[Indices] = input.knownIdentifier.flatMap(MockStorage.getIndicesByKnownIdentifier)
    // TODO: None.get
    val inputAttestation = input.attestation.get
    val attestations: Either[TransactionSyntaxError, Attestation] =
      inputAttestation.value match {
        case Attestation.Value.Predicate(Attestation.Predicate(Some(predLock), responses, _)) =>
          if (predLock.challenges.length != responses.length)
            Left(TransactionSyntaxErrors.AttestationMalformed(inputAttestation))
          else
            Right(
              Attestation().withPredicate(
                Attestation.Predicate(predLock.some, predLock.challenges.map(getProof(msg, _, idx).getOrElse(Proof())))
              )
            )

        case _ => ??? // We are not handling other types of Attestations at this moment in time
      }

    attestations.map(_.some).map(SpentTransactionOutput(input.knownIdentifier, _, input.value, input.datum, input.opts))
  }

  /**
   * Prove a transaction. That is, prove all the inputs within the transaction if possible
   *
   * If not possible, errors for the unprovable inputs are returned
   *
   * @param unprovenTx The unproven transaction to prove
   * @return The proven version of the transaction. If not possible, errors for the unprovable inputs are returned
   */
  override def prove(unprovenTx: IoTransaction): Either[List[TransactionSyntaxError], IoTransaction] = {
    val signable = ioTransactionSignable.signableBytes(unprovenTx)
    val (errs, provenInputs) = unprovenTx.inputs.toList
      .partitionMap(proveInput(_, signable))

    if (errs.isEmpty && provenInputs.nonEmpty) Right(IoTransaction(provenInputs, unprovenTx.outputs, unprovenTx.datum))
    else Left(errs)
  }

  /**
   * Validate whether the transaction is authorized. That is, all contained attestations are satisfied
   * @param tx Transaction to validate
   * @param ctx Context to validate the transaction in
   * @return Iff transaction is authorized
   */
  override def validate(tx: IoTransaction, ctx: Context): List[TransactionAuthorizationError] = {
    implicit val verifier: Verifier[Id, Datum] = Verifier.instances.verifierInstance
    TransactionAuthorizationInterpreter
      .make[Id]()
      .validate(ctx)(tx)
      .swap // Swap to get the errors
      .toList
  }

  /**
   * Prove and validate a transaction.
   * That is, attempt to prove all the inputs within the transaction and validate if the transaction is successfully proven
   *
   * @param unprovenTx The unproven transaction to prove
   * @return The proven version of the input is successfully proven. Else a validation error
   */
  override def proveAndValidate(unprovenTx: IoTransaction, ctx: Context): Either[List[ValidationError], IoTransaction] =
    prove(unprovenTx) match {
      case Right(provenTx) =>
        validate(provenTx, ctx) match {
          case Nil                         => Right(provenTx)
          case errs: List[ValidationError] => Left(errs)
        }
      case Left(errs) => Left(errs)
    }
}
