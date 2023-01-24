package co.topl.brambl.routines.signatures.validators

import cats.Id
import co.topl.brambl.routines.Routine
import co.topl.crypto.{signatures, PublicKey}
import co.topl.quivr.algebras.SignatureVerifier
import co.topl.quivr.runtime.{QuivrRuntimeError, QuivrRuntimeErrors}
import quivr.models.SignatureVerification

object Curve25519SignatureInterpreter extends SignatureVerifier[Id] with Routine {
  override val routine: String = "curve25519"

  override def validate(v: SignatureVerification): Id[Either[QuivrRuntimeError, SignatureVerification]] =
    if (
      signatures.Curve25519.verify(
        signatures.Signature(v.signature.get.value.toByteArray),
        v.message.get.value.toByteArray,
        PublicKey(v.verificationKey.get.value.toByteArray)
      )
    ) Right(v)
    else Left(QuivrRuntimeErrors.ValidationError.LockedPropositionIsUnsatisfiable)
}
