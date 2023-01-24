package co.topl.brambl.validation

import co.topl.brambl.models.transaction.Attestation

sealed abstract class TransactionSyntaxError extends ValidationError

object TransactionSyntaxError {

  /**
   * Error for when a transaction is not syntactically valid because its attestation is formed incorrectly
   */
  case class AttestationMalformed(attestation: Attestation) extends TransactionSyntaxError
}
