package co.topl.brambl.routines.digests.validators

import cats.Id
import co.topl.brambl.routines.Routine
import co.topl.crypto.hash.Blake2b256
import co.topl.quivr.algebras.DigestVerifier
import co.topl.quivr.runtime.{QuivrRuntimeError, QuivrRuntimeErrors}
import quivr.models.{Digest, DigestVerification, Preimage}
import scodec.bits.ByteVector

object Blake2b256DigestInterpreter extends DigestVerifier[Id] with Routine {
  override val routine: String = "blake2b256"

  override def validate(v: DigestVerification): Id[Either[QuivrRuntimeError, DigestVerification]] = v match {
    case DigestVerification(Some(Digest(Digest.Value.Digest32(d), _)), Some(Preimage(p, salt, _)), _) =>
      val testHash: ByteVector = (new Blake2b256).hash(ByteVector(p.toByteArray ++ salt.toByteArray))
      val expectedHash = ByteVector(d.toByteArray)
      if (testHash === expectedHash)
        Right(v)
      else
        Left(QuivrRuntimeErrors.ValidationError.LockedPropositionIsUnsatisfiable) // TODO: replace with correct error. DigestIsInvalid?

    case _ => Left(QuivrRuntimeErrors.ValidationError.LockedPropositionIsUnsatisfiable) // TODO: replace with correct error
  }
}
