package co.topl.brambl.display

import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.validation.TransactionAuthorizationError.AuthorizationFailed
import co.topl.brambl.validation.{TransactionSyntaxError, ValidationError}
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

  implicit val validationErrorDisplay: DisplayOps[ValidationError] = {
    case err: TransactionSyntaxError => err.display
    case err: AuthorizationFailed    => err.display
    case _                           => "Unknown validation error" // Should not get here
  }

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
      Seq(
        "Proof does not satisfy proposition.",
        displayIndent("Proposition:", Indent),
        displayIndent(proposition.display, Indent),
        displayIndent("Proof:", Indent),
        displayIndent(proof.display, Indent)
      ).mkString("\n")
    case _ => "Unknown Quivr Runtime error" // Should not get here
  }
}
