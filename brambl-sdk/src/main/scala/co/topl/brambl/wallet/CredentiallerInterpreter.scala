package co.topl.brambl.wallet

import cats.{Applicative, Monad}
import cats.implicits._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.validation.{TransactionAuthorizationInterpreter, TransactionSyntaxInterpreter, ValidationError}
import co.topl.brambl.Context
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.quivr.api.Prover
import co.topl.quivr.api.Verifier.instances.verifierInstance
import quivr.models.{KeyPair, Proof, Proposition, SignableBytes, Witness}
import co.topl.brambl.models.Indices
import cats.data.EitherT
import co.topl.brambl.dataApi.WalletStateAlgebra
import co.topl.brambl.models.box.Attestation
import co.topl.crypto.signing.ExtendedEd25519
import com.google.protobuf.ByteString

object CredentiallerInterpreter {

  def make[F[_]: Monad](
    walletApi:      WalletApi[F],
    walletStateApi: WalletStateAlgebra[F],
    mainKey:        KeyPair
  ): Credentialler[F] = new Credentialler[F] {
    require(mainKey.vk.vk.isExtendedEd25519, "mainKey must be an extended Ed25519 key")
    require(mainKey.sk.sk.isExtendedEd25519, "mainKey must be an extended Ed25519 key")

    override def prove(unprovenTx: IoTransaction): F[IoTransaction] = {
      val signable = unprovenTx.signable
      unprovenTx.inputs
        .map(proveInput(_, signable))
        .sequence
        .map(unprovenTx.withInputs)
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

    /**
     * Return a Proof that will satisfy a Proposition and signable bytes, if possible. Any unprovable leaf (non-composite)
     * Propositions will result in a [[Proof.Value.Empty]].
     * Leaf/Atomic/Non-composite Propositions are: Locked, Digest, Signature, Height, and Tick
     * If there are valid existing proofs for any leaf Propositions, they should not be overwritten.
     *
     * It may not be possible to retrieve a proof if
     * - The proposition type is not yet supported
     * (not one of Locked, Digest, Signature, Height, Tick, Threshold, And, Or, and Not)
     * - The secret data required for the proof is not available (idx for signature, preimage for digest)
     * - The signature routine is not supported (not ExtendedEd25519)
     *
     * @param msg           Signable bytes to bind to the proof
     * @param prop   Proposition in which the Proof should satisfy
     * @param existingProof Existing proof of the proposition
     * @return The Proof
     */
    private def getProof(msg: SignableBytes, prop: Proposition, existingProof: Proof): F[Proof] = prop.value match {
      case Proposition.Value.Locked(_)                            => getLockedProof(existingProof, msg)
      case Proposition.Value.HeightRange(_)                       => getHeightProof(existingProof, msg)
      case Proposition.Value.TickRange(_)                         => getTickProof(existingProof, msg)
      case Proposition.Value.Digest(digest)                       => getDigestProof(existingProof, msg, digest)
      case Proposition.Value.DigitalSignature(signature)          => getSignatureProof(existingProof, msg, signature)
      case Proposition.Value.Not(Proposition.Not(not, _))         => getNotProof(existingProof, msg, not)
      case Proposition.Value.And(Proposition.And(left, right, _)) => getAndProof(existingProof, msg, left, right)
      case Proposition.Value.Or(Proposition.Or(left, right, _))   => getOrProof(existingProof, msg, left, right)
      case Proposition.Value.Threshold(Proposition.Threshold(challenges, _, _)) =>
        getThresholdProof(existingProof, msg, challenges)
      case _ => Proof().pure[F]
    }

    /**
     * Return a Proof that will satisfy a Locked proposition and signable bytes.
     * Since this is a non-composite (leaf) type, if there is a valid existing proof (non-empty and same type), it will
     * be used. Otherwise, a new proof will be generated.
     *
     * @param existingProof Existing proof of the proposition
     * @param msg           Signable bytes to bind to the proof
     * @return The Proof
     */
    private def getLockedProof(existingProof: Proof, msg: SignableBytes): F[Proof] =
      if (existingProof.value.isLocked) existingProof.pure[F] else Prover.lockedProver[F].prove((), msg)

    /**
     * Return a Proof that will satisfy a Height Range proposition and signable bytes.
     * Since this is a non-composite (leaf) type, if there is a valid existing proof (non-empty and same type), it will
     * be used. Otherwise, a new proof will be generated.
     *
     * @param existingProof Existing proof of the proposition
     * @param msg           Signable bytes to bind to the proof
     * @return The Proof
     */
    private def getHeightProof(existingProof: Proof, msg: SignableBytes): F[Proof] =
      if (existingProof.value.isHeightRange) existingProof.pure[F] else Prover.heightProver[F].prove((), msg)

    /**
     * Return a Proof that will satisfy a Tick Range proposition and signable bytes.
     * Since this is a non-composite (leaf) type, if there is a valid existing proof (non-empty and same type), it will
     * be used. Otherwise, a new proof will be generated.
     *
     * @param existingProof Existing proof of the proposition
     * @param msg           Signable bytes to bind to the proof
     * @return The Proof
     */
    private def getTickProof(existingProof: Proof, msg: SignableBytes): F[Proof] =
      if (existingProof.value.isTickRange) existingProof.pure[F] else Prover.tickProver[F].prove((), msg)

    /**
     * Return a Proof that will satisfy a Digest proposition and signable bytes.
     * Since this is a non-composite (leaf) type, if there is a valid existing proof (non-empty and same type), it will
     * be used. Otherwise, a new proof will be generated. If the digest proposition is unable to be proven, an empty
     * proof will be returned.
     *
     * @param existingProof Existing proof of the proposition
     * @param msg           Signable bytes to bind to the proof
     * @param digest        The Digest Proposition to prove
     * @return The Proof
     */
    private def getDigestProof(existingProof: Proof, msg: SignableBytes, digest: Proposition.Digest): F[Proof] =
      if (existingProof.value.isDigest) existingProof.pure[F]
      else
        walletStateApi
          .getPreimage(digest)
          .flatMap(
            _.map(preimage => Prover.digestProver[F].prove(preimage, msg)).getOrElse(Proof().pure[F])
          )

    /**
     * Return a Proof that will satisfy a Digital Signature proposition and signable bytes.
     * Since this is a non-composite (leaf) type, if there is a valid existing proof (non-empty and same type), it will
     * be used. Otherwise, a new proof will be generated. If the signature proposition is unable to be proven, an empty
     * proof will be returned.
     *
     * @param existingProof Existing proof of the proposition
     * @param msg           Signable bytes to bind to the proof
     * @param signature     The Signature Proposition to prove
     * @return The Proof
     */
    private def getSignatureProof(
      existingProof: Proof,
      msg:           SignableBytes,
      signature:     Proposition.DigitalSignature
    ): F[Proof] =
      if (existingProof.value.isDigitalSignature) existingProof.pure[F]
      else
        walletStateApi
          .getIndicesBySignature(signature)
          .flatMap(
            _.map(idx => getSignatureProofForRoutine(signature.routine, idx, msg)).getOrElse(Proof().pure[F])
          )

    /**
     * Return a Signature Proof for a given signing routine with a signature of msg using the signing key at idx, if
     * possible. Otherwise return [[Proof.Value.Empty]]
     *
     * It may not be possible to generate a signature proof if the signature routine is not supported. We currently
     * support only ExtendedEd25519.
     *
     * @param routine Signature routine to use
     * @param idx     Indices for which the proof's secret data can be obtained from
     * @param msg     Signable bytes to bind to the proof
     * @return The Proof
     */
    private def getSignatureProofForRoutine(routine: String, idx: Indices, msg: SignableBytes): F[Proof] =
      routine match {
        case "ExtendedEd25519" =>
          walletApi
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
     * Return a Proof that will satisfy a Not proposition and signable bytes.
     * Since this is a composite type, even if a correct-type existing outer proof is provided, the inner proposition
     * may need to be proven recursively.
     *
     * @param existingProof Existing proof of the Not proposition
     * @param msg           Signable bytes to bind to the proof
     * @param innerProposition  The inner Proposition contained in the Not Proposition to prove
     * @return The Proof
     */
    private def getNotProof(existingProof: Proof, msg: SignableBytes, innerProposition: Proposition): F[Proof] = {
      val innerProof = existingProof.value match {
        case Proof.Value.Not(Proof.Not(_, p, _)) => p
        case _                                   => Proof()
      }
      getProof(msg, innerProposition, innerProof).flatMap(Prover.notProver[F].prove(_, msg))
    }

    /**
     * Return a Proof that will satisfy an And proposition and signable bytes.
     * Since this is a composite type, even if a correct-type existing outer proof is provided, the inner propositions
     * may need to be proven recursively.
     *
     * @param existingProof    Existing proof of the And proposition
     * @param msg              Signable bytes to bind to the proof
     * @param leftProposition  An inner Proposition contained in the And Proposition to prove
     * @param rightProposition An inner Proposition contained in the And Proposition to prove
     * @return The Proof
     */
    private def getAndProof(
      existingProof:    Proof,
      msg:              SignableBytes,
      leftProposition:  Proposition,
      rightProposition: Proposition
    ): F[Proof] = {
      val (leftProof, rightProof) = existingProof.value match {
        case Proof.Value.And(Proof.And(_, leftProof, rightProof, _)) => (leftProof, rightProof)
        case _                                                       => (Proof(), Proof())
      }
      Applicative[F]
        .map2(getProof(msg, leftProposition, leftProof), getProof(msg, rightProposition, rightProof))(
          (leftProof, rightProof) => Prover.andProver[F].prove((leftProof, rightProof), msg)
        )
        .flatten
    }

    /**
     * Return a Proof that will satisfy an Or proposition and signable bytes.
     * Since this is a composite type, even if a correct-type existing outer proof is provided, the inner propositions
     * may need to be proven recursively.
     *
     * @param existingProof    Existing proof of the Or proposition
     * @param msg              Signable bytes to bind to the proof
     * @param leftProposition  An inner Proposition contained in the Or Proposition to prove
     * @param rightProposition An inner Proposition contained in the Or Proposition to prove
     * @return The Proof
     */
    private def getOrProof(
      existingProof:    Proof,
      msg:              SignableBytes,
      leftProposition:  Proposition,
      rightProposition: Proposition
    ): F[Proof] = {
      val (leftProof, rightProof) = existingProof.value match {
        case Proof.Value.Or(Proof.Or(_, leftProof, rightProof, _)) => (leftProof, rightProof)
        case _                                                     => (Proof(), Proof())
      }
      Applicative[F]
        .map2(getProof(msg, leftProposition, leftProof), getProof(msg, rightProposition, rightProof))(
          (leftProof, rightProof) => Prover.orProver[F].prove((leftProof, rightProof), msg)
        )
        .flatten
    }

    /**
     * Return a Proof that will satisfy a Threshold proposition and signable bytes.
     * Since this is a composite type, even if a correct-type existing outer proof is provided, the inner propositions
     * may need to be proven recursively.
     *
     * @param existingProof     Existing proof of the Threshold proposition
     * @param msg               Signable bytes to bind to the proof
     * @param innerPropositions Inner Propositions contained in the Threshold Proposition to prove
     * @return The Proof
     */
    private def getThresholdProof(
      existingProof:     Proof,
      msg:               SignableBytes,
      innerPropositions: Seq[Proposition]
    ): F[Proof] = {
      val responses = existingProof.value match {
        case Proof.Value.Threshold(Proof.Threshold(_, responses, _)) => responses
        case _                                                       => List.fill(innerPropositions.length)(Proof())
      }
      innerPropositions
        .zip(responses)
        .map(pair => getProof(msg, pair._1, pair._2))
        .sequence
        .flatMap(proofs => Prover.thresholdProver[F].prove(proofs.toSet, msg))
    }
  }
}
