package co.topl.brambl.validation

import cats.Monad
import co.topl.crypto.hash.Blake2b256
import co.topl.quivr.algebras.DigestVerifier
import co.topl.quivr.runtime.QuivrRuntimeError
import co.topl.quivr.runtime.QuivrRuntimeErrors.ValidationError
import quivr.models.{Digest, DigestVerification, Preimage}
import scodec.bits.ByteVector
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEitherObject}

/**
 * Validates that a Blake2b256 digest is valid.
 */
object Blake2b256DigestInterpreter {

  def make[F[_]: Monad](): DigestVerifier[F] = new DigestVerifier[F] {

    /**
     * Validates that an Blake2b256 digest is valid.
     * @param t DigestVerification object containing the digest and preimage
     * @return The DigestVerification object if the digest is valid, otherwise an error
     */
    override def validate(t: DigestVerification): F[Either[QuivrRuntimeError, DigestVerification]] = t match {
      case DigestVerification(Digest(Digest.Value.Digest32(d), _), Preimage(p, salt, _), _) =>
        val testHash: ByteVector = (new Blake2b256).hash(ByteVector(p.toByteArray ++ salt.toByteArray))
        val expectedHash = ByteVector(d.value.toByteArray)
        if (testHash === expectedHash)
          Either.right[QuivrRuntimeError, DigestVerification](t).pure[F]
        else // TODO: replace with correct error. Verification failed.
          Either.left[QuivrRuntimeError, DigestVerification](ValidationError.LockedPropositionIsUnsatisfiable).pure[F]
      // TODO: replace with correct error. SignatureVerification is malformed
      case _ =>
        Either.left[QuivrRuntimeError, DigestVerification](ValidationError.LockedPropositionIsUnsatisfiable).pure[F]
    }
  }
}
