package co.topl.brambl.validation

import co.topl.brambl.models.KnownIdentifier
import co.topl.brambl.models.transaction.Attestation

sealed abstract class TransactionSyntaxError extends ValidationError

object TransactionSyntaxError {

  /**
   * Generic error for when a transaction is not syntactically valid
   *
   * TEMPORARY until return type (and its contents) of Syntax validation is finalized
   */
  case object SyntaxFailed extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction does not contain at least 1 input.
  * */
  case object EmptyInputs extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction multiple inputs referring to the same KnownIdentifier.
   * */
  case class DuplicateInput(knownIdentifier: KnownIdentifier) extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction contains too many outputs.
   * */
  case object ExcessiveOutputsCount extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction contains an invalid timestamp.
   * */
  case class InvalidTimestamp(timestamp: Long) extends TransactionSyntaxError

  /**
   * Error for when a transaction is not syntactically valid because its attestation is formed incorrectly
   */
  case class AttestationMalformed(attestation: Attestation) extends TransactionSyntaxError
}
