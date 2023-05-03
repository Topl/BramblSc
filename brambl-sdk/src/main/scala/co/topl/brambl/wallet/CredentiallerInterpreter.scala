package co.topl.brambl.wallet

import cats.{Applicative, Monad}
import cats.implicits._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.validation.{TransactionAuthorizationInterpreter, TransactionSyntaxInterpreter, ValidationError}
import co.topl.brambl.Context
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.dataApi.DataApi
import co.topl.quivr.api.Prover
import co.topl.quivr.api.Verifier.instances.verifierInstance
import quivr.models.{KeyPair, Proof, Proposition, SignableBytes, Witness}
import co.topl.brambl.models.Indices
import cats.data.EitherT
import co.topl.brambl.models.box.Attestation
import co.topl.crypto.signing.ExtendedEd25519
import com.google.protobuf.ByteString

object CredentiallerInterpreter {

  def make[F[_]: Monad](dataApi: DataApi[F], mainKey: KeyPair): Credentialler[F] = new Credentialler[F] {
    require(mainKey.vk.vk.isExtendedEd25519, "mainKey must be an extended Ed25519 key")
    require(mainKey.sk.sk.isExtendedEd25519, "mainKey must be an extended Ed25519 key")

    override def prove(unprovenTx: IoTransaction): F[IoTransaction] = {
      val signable = unprovenTx.signable
      unprovenTx.inputs
        .map(proveInput(_, signable))
        .sequence
        .map(IoTransaction.defaultInstance.withInputs(_).withOutputs(unprovenTx.outputs).withDatum(unprovenTx.datum))
    }

    override def validate(tx: IoTransaction, ctx: Context[F]): F[List[ValidationError]] = for {
      syntaxErrs <- EitherT(TransactionSyntaxInterpreter.make[F]().validate(tx)).swap
        .map(_.toList)
        .valueOr(_ => List.empty)
      authErrs <- EitherT(TransactionAuthorizationInterpreter.make[F]().validate(ctx)(tx)).swap
        .map(List(_))
        .valueOr(_ => List.empty)
    } yield syntaxErrs ++ authErrs

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
     * Any unprovable leaf (non-composite) Propositions will result in a [[Proof.Value.Empty]].
     * Leaf/Atomic/Non-composite Propositions are: Locked, Digest, Signature, Height, and Tick
     * If there are valid existing proofs for any leaf Propositions, they should not be overwritten.
     *
     * It may not be possible to retrieve a proof if
     * - The proposition type is not yet supported
     *      (not one of Locked, Digest, Signature, Height, Tick, Threshold, And, Or, and Not)
     * - The secret data required for the proof is not available (idx for signature, preimage for digest)
     * - The signature routine is not supported (not ExtendedEd25519)
     *
     * @param msg         Signable bytes to bind to the proof
     * @param proposition Proposition in which the Proof should satisfy
     * @param existingProof    Existing proof of the proposition
     * @return The Proof
     */
    private def getProof(msg: SignableBytes, proposition: Proposition, existingProof: Proof): F[Proof] =
      proposition.value match {
        // Atomic/leaf propositions; if a non-empty same type proof is provided, return it. If not, try to generate one
        case Proposition.Value.Locked(_) =>
          if (existingProof.value.isLocked) existingProof.pure[F] else Prover.lockedProver[F].prove((), msg)
        case Proposition.Value.HeightRange(_) =>
          if (existingProof.value.isHeightRange) existingProof.pure[F] else Prover.heightProver[F].prove((), msg)
        case Proposition.Value.TickRange(_) =>
          if (existingProof.value.isTickRange) existingProof.pure[F] else Prover.tickProver[F].prove((), msg)
        case Proposition.Value.Digest(digest) =>
          if (existingProof.value.isDigest) existingProof.pure[F]
          else
            dataApi
              .getPreimage(digest)
              .flatMap(
                _.toOption.map(preimage => Prover.digestProver[F].prove(preimage, msg)).getOrElse(Proof().pure[F])
              )
        case Proposition.Value.DigitalSignature(signature) =>
          if (existingProof.value.isDigitalSignature) existingProof.pure[F]
          else
            dataApi
              .getIndices(signature)
              .flatMap(_.toOption.map(idx => getSignatureProof(signature.routine, idx, msg)).getOrElse(Proof().pure[F]))
        // Composite propositions; even if a correct-type outer proof is provided, the inner propositions may need to be proven
        case Proposition.Value.Not(Proposition.Not(not, _)) =>
          val innerProof = existingProof.value match {
            case Proof.Value.Not(Proof.Not(_, p, _)) => p
            case _                                   => Proof()
          }
          getProof(msg, not, innerProof).flatMap(Prover.notProver[F].prove(_, msg))
        case Proposition.Value.And(Proposition.And(left, right, _)) =>
          val (leftProof, rightProof) = existingProof.value match {
            case Proof.Value.And(Proof.And(_, leftProof, rightProof, _)) => (leftProof, rightProof)
            case _                                                       => (Proof(), Proof())
          }
          Applicative[F]
            .map2(getProof(msg, left, leftProof), getProof(msg, right, rightProof))((leftProof, rightProof) =>
              Prover.andProver[F].prove((leftProof, rightProof), msg)
            )
            .flatten
        case Proposition.Value.Or(Proposition.Or(left, right, _)) =>
          val (leftProof, rightProof) = existingProof.value match {
            case Proof.Value.Or(Proof.Or(_, leftProof, rightProof, _)) => (leftProof, rightProof)
            case _                                                     => (Proof(), Proof())
          }
          Applicative[F]
            .map2(getProof(msg, left, leftProof), getProof(msg, right, rightProof))((leftProof, rightProof) =>
              Prover.orProver[F].prove((leftProof, rightProof), msg)
            )
            .flatten
        case Proposition.Value.Threshold(Proposition.Threshold(challenges, _, _)) =>
          val responses = existingProof.value match {
            case Proof.Value.Threshold(Proof.Threshold(_, responses, _)) => responses
            case _                                                       => List.fill(challenges.length)(Proof())
          }
          challenges
            .zip(responses)
            .map(pair => getProof(msg, pair._1, pair._2))
            .sequence
            .flatMap(proofs => Prover.thresholdProver[F].prove(proofs.toSet, msg))
        case _ => Proof().pure[F]
      }

    /**
     * Return a Signature Proof that will satisfy a Signature Proposition, if possible.
     * Otherwise return [[Proof.Value.Empty]]
     *
     * It may not be possible to generate a signature proof if the signature routine is not supported. We currently
     * support only ExtendedEd25519.
     *
     * @param routine     Signature routine to use
     * @param idx         Indices for which the proof's secret data can be obtained from
     * @param msg         Signable bytes to bind to the proof
     * @return The Proof
     */
    private def getSignatureProof(routine: String, idx: Indices, msg: SignableBytes): F[Proof] = routine match {
      case "ExtendedEd25519" =>
        WalletApi
          .make[F](dataApi)
          .deriveChildKeys(mainKey, idx)
          .map(WalletApi.pbKeyPairToCryotoKeyPair)
          .flatMap(kp =>
            Prover
              .signatureProver[F]
              .prove(
                Witness(ByteString.copyFrom((new ExtendedEd25519).sign(kp.signingKey, msg.value.toByteArray))),
                msg
              )
          )
      case _ => Proof().pure[F]
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
      val attestation: F[Attestation] = input.attestation.value match {
        case Attestation.Value.Predicate(Attestation.Predicate(predLock, proofs, _)) =>
          predLock.challenges
            // TODO: Fix .getRevealed
            .map(_.getRevealed)
            .zip(proofs)
            .map(pair => getProof(msg, pair._1, pair._2))
            .sequence
            .map(proofs => Attestation().withPredicate(Attestation.Predicate(predLock, proofs)))
        // TODO: We are not handling other types of Attestations at this moment in time
        case _ => ???
      }
      attestation.map(SpentTransactionOutput(input.address, _, input.value))
    }
  }
}
