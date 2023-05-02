package co.topl.brambl.wallet

import cats.Monad
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
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._

object CredentiallerInterpreter {

  def make[F[_]: Monad](dataApi: DataApi[F], mainKey: KeyPair): Credentialler[F] = new Credentialler[F] {
    require(mainKey.vk.vk.isExtendedEd25519, "mainKey must be an extended Ed25519 key")
    require(mainKey.sk.sk.isExtendedEd25519, "mainKey must be an extended Ed25519 key")

    override def prove(unprovenTx: IoTransaction): F[IoTransaction] = {
      val signable = unprovenTx.signable
      unprovenTx.inputs
        .map(proveInput(_, signable))
        .sequence
        .map(IoTransaction(_, unprovenTx.outputs, unprovenTx.datum))
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
     * Otherwise return [[Proof.Value.Empty]]
     *
     * It may not be possible to retrieve a proof if
     * - The proposition type is not yet supported (not one of Locked, Digest, Signature, Height and Tick)
     * - The secret data required for the proof is not available (idx for signature, preimage for digest)
     * - The signature routine is not supported (not ExtendedEd25519)
     *
     * @param msg         Signable bytes to bind to the proof
     * @param proposition Proposition in which the Proof should satisfy
     * @return The Proof
     */
    private def getProof(msg: SignableBytes, proposition: Proposition): F[Proof] = proposition.value match {
      case _: Proposition.Value.Locked      => Prover.lockedProver[F].prove((), msg)
      case _: Proposition.Value.HeightRange => Prover.heightProver[F].prove((), msg)
      case _: Proposition.Value.TickRange   => Prover.tickProver[F].prove((), msg)
      case Proposition.Value.Digest(digest) =>
        dataApi
          .getPreimage(digest)
          .flatMap(_.toOption.map(preimage => Prover.digestProver[F].prove(preimage, msg)).getOrElse(Proof().pure[F]))
      case Proposition.Value.DigitalSignature(signature) =>
        dataApi
          .getIndices(signature)
          .flatMap(_.toOption.map(idx => getSignatureProof(signature.routine, idx, msg)).getOrElse(Proof().pure[F]))
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
        case Attestation.Value.Predicate(Attestation.Predicate(predLock, _, _)) =>
          predLock.challenges
            // TODO: Fix .getRevealed
            .map(_.getRevealed)
            .map(getProof(msg, _))
            .sequence
            .map(proofs => Attestation().withPredicate(Attestation.Predicate(predLock, proofs)))
        // TODO: We are not handling other types of Attestations at this moment in time
        case _ => ???
      }
      attestation.map(SpentTransactionOutput(input.address, _, input.value))
    }
  }
}
