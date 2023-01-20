package co.topl.brambl.routines.signatures.validators

import cats.Id
import co.topl.brambl.routines.Routine
import co.topl.crypto.signing.Ed25519
import co.topl.quivr.algebras.SignatureVerifier
import co.topl.quivr.runtime.{QuivrRuntimeError, QuivrRuntimeErrors}
import quivr.models.{Message, SignatureVerification, VerificationKey, Witness}
import scodec.bits.ByteVector

object Ed25519SignatureInterpreter extends SignatureVerifier[Id] with Routine {
  override val routine: String = "ed25519"

  override def validate(v: SignatureVerification): Id[Either[QuivrRuntimeError, SignatureVerification]] =  v match {
    case SignatureVerification(Some(VerificationKey(vk, _)), Some(Witness(sig, _)), Some(Message(msg, _)), _) =>
      if(Ed25519.instance.verify(ByteVector(sig.toByteArray), ByteVector(msg.toByteArray), ByteVector(vk.toByteArray)))
        Right(v)
      else
        Left(QuivrRuntimeErrors.ValidationError.LockedPropositionIsUnsatisfiable) // TODO: replace with correct error. SignatureIsInvalid?

    case _ => Left(QuivrRuntimeErrors.ValidationError.LockedPropositionIsUnsatisfiable) // TODO: replace with correct error
  }
}
