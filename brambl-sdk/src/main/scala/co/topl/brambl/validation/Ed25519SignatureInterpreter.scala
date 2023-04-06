package co.topl.brambl.validation

import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEitherObject}
import cats.Monad
import co.topl.crypto.signing.Ed25519
import co.topl.quivr.algebras.SignatureVerifier
import co.topl.quivr.runtime.QuivrRuntimeError
import co.topl.quivr.runtime.QuivrRuntimeErrors.ValidationError
import quivr.models.{Message, SignatureVerification, VerificationKey, Witness}
import quivr.models.VerificationKey.{Ed25519VerificationKey, Value}

/**
 * Validates that an Ed25519 signature is valid.
 */
object Ed25519SignatureInterpreter {

  def make[F[_]: Monad](): SignatureVerifier[F] = new SignatureVerifier[F] {

    /**
     * Validates that an Ed25519 signature is valid.
     * @param t SignatureVerification object containing the message, verification key, and signature
     * @return The SignatureVerification object if the signature is valid, otherwise an error
     */
    override def validate(t: SignatureVerification): F[Either[QuivrRuntimeError, SignatureVerification]] = t match {
      case SignatureVerification(
            VerificationKey(Value.Ed25519(Ed25519VerificationKey(vk, _)), _),
            Witness(sig, _),
            Message(msg, _),
            _
          ) =>
        if ((new Ed25519).verify(sig.toByteArray, msg.toByteArray, Ed25519.PublicKey(vk.toByteArray)))
          Either.right[QuivrRuntimeError, SignatureVerification](t).pure[F]
        else // TODO: replace with correct error. Verification failed.
          Either
            .left[QuivrRuntimeError, SignatureVerification](ValidationError.LockedPropositionIsUnsatisfiable)
            .pure[F]
      // TODO: replace with correct error. SignatureVerification is malformed
      case _ =>
        Either.left[QuivrRuntimeError, SignatureVerification](ValidationError.LockedPropositionIsUnsatisfiable).pure[F]
    }
  }
}
