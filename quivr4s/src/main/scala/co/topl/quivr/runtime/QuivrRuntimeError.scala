package co.topl.quivr.runtime

import quivr.models.Proof
import quivr.models.Proposition

/**
 * Errors resulting from evaluation of Quivr propositions / proofs
 */
sealed abstract class QuivrRuntimeError

object QuivrRuntimeErrors {

  /**
   * A Validation error indicates that the evaluation of the proof failed for the given proposition within the provided context.
   */
  object ValidationError {
    final case class EvaluationAuthorizationFailed(proposition: Proposition, proof: Proof) extends QuivrRuntimeError
    final case class MessageAuthorizationFailed(proof: Proof) extends QuivrRuntimeError
    final case object LockedPropositionIsUnsatisfiable extends QuivrRuntimeError
    final case object UserProvidedInterfaceFailure extends QuivrRuntimeError
  }

  /**
   * A Context error indicates that the Dynamic context failed to retrieve an instance of a requested member
   */
  object ContextError {
    final case object FailedToFindDigestVerifier extends QuivrRuntimeError
    final case object FailedToFindSignatureVerifier extends QuivrRuntimeError
    final case object FailedToFindDatum extends QuivrRuntimeError
    final case object FailedToFindInterface extends QuivrRuntimeError
  }
}
