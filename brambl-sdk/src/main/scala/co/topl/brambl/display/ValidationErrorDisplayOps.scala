package co.topl.brambl.display

import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.validation.TransactionAuthorizationError.AuthorizationFailed
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.quivr.runtime.QuivrRuntimeError
import co.topl.quivr.runtime.QuivrRuntimeErrors.ContextError.{
  FailedToFindDatum,
  FailedToFindDigestVerifier,
  FailedToFindInterface,
  FailedToFindSignatureVerifier
}
import co.topl.quivr.runtime.QuivrRuntimeErrors.ValidationError.{
  EvaluationAuthorizationFailed,
  LockedPropositionIsUnsatisfiable,
  MessageAuthorizationFailed,
  UserProvidedInterfaceFailure
}

trait ValidationErrorDisplayOps {

  implicit val syntaxErrorDisplay: DisplayOps[TransactionSyntaxError] = (err: TransactionSyntaxError) =>
    "Not Implemented Yet"

  implicit val authorizationErrorDisplay: DisplayOps[AuthorizationFailed] = (err: AuthorizationFailed) =>
    s"Authorization failed. Causes:\n" + err.errors.map("- " + _.display).mkString("\n")

  implicit val quivrErrorDisplay: DisplayOps[QuivrRuntimeError] = {
    case FailedToFindDigestVerifier        => "Failed to find digest verifier"
    case FailedToFindSignatureVerifier     => "Failed to find signature verifier"
    case FailedToFindDatum                 => "Failed to find datum"
    case FailedToFindInterface             => "Failed to find interface"
    case UserProvidedInterfaceFailure      => "User provided interface failure"
    case LockedPropositionIsUnsatisfiable  => "Locked proposition is unsatisfiable"
    case MessageAuthorizationFailed(proof) => s"Transaction Bind on proof is invalid. Proof: ${proof.display}"
    case EvaluationAuthorizationFailed(proposition, proof) =>
      s"Proof does not satisfy proposition. Proposition: ${proposition.display}, Proof: ${proof.display}"
    case _ => "Unknown Quivr Runtime error" // Should not get here
  }
}
