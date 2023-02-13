package co.topl.brambl.validation

import co.topl.quivr.runtime.QuivrRuntimeError

sealed abstract class TransactionAuthorizationError extends ValidationError

object TransactionAuthorizationError {

  /**
   * Generic error for when a transaction is not authorized
   *
   * Temporary until the interpreter utilized Contextual and Permanent errors
   *
   * @param errors the errors that occurred during the authorization process, if available
   */
  case class AuthorizationFailed(errors: List[QuivrRuntimeError] = List.empty) extends TransactionAuthorizationError

  /**
   * An Authorization error indicating that this transaction was invalid only within the provided validation context.
   * It _might_ become valid later (or perhaps it _was_ valid previously)
   * (i.e. height lock)
   */
  case class Contextual(error: QuivrRuntimeError) extends TransactionAuthorizationError

  /**
   * An Authorization error indicating that this transaction will never be valid.  This is usually the result of something
   * that could _probably_ be determined in a syntactic validation check, meaning no context is needed to perform the
   * authorization validation.  (i.e. invalid signature)
   */
  case class Permanent(error: QuivrRuntimeError) extends TransactionAuthorizationError
}
