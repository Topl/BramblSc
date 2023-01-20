package co.topl.brambl.routines.digests.validators

import cats.Id
import co.topl.brambl.routines.Routine
import co.topl.crypto.hash.blake2b256
import co.topl.quivr.algebras.DigestVerifier
import co.topl.quivr.runtime.{QuivrRuntimeError, QuivrRuntimeErrors}
import quivr.models.DigestVerification

object Blake2b256DigestInterpreter extends DigestVerifier[Id] with Routine {
  override val routine: String = "blake2b256"

  override def validate(v: DigestVerification): Id[Either[QuivrRuntimeError, DigestVerification]] = {
    val test = blake2b256.hash(v.preimage.get.input.toByteArray ++ v.preimage.get.salt.toByteArray).value
    if (v.digest.get.value.digest32.get.value.toByteArray.sameElements(test)) Right(v)
    else Left(QuivrRuntimeErrors.ValidationError.LockedPropositionIsUnsatisfiable)
  }
}
