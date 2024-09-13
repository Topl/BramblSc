package co.topl.brambl.validation

import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box.{AssetMergingStatement, Value}
import co.topl.brambl.models.transaction.Schedule
import quivr.models.{Proof, Proposition}

sealed abstract class TransactionSyntaxError extends ValidationError

object TransactionSyntaxError {

  /**
   * A Syntax error indicating that this transaction does not contain at least 1 input.
   */
  case object EmptyInputs extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction multiple inputs referring to the same KnownIdentifier.
   */
  case class DuplicateInput(knownIdentifier: TransactionOutputAddress) extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction contains too many outputs.
   */
  case object ExcessiveOutputsCount extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction contains an invalid timestamp.
   */
  case class InvalidTimestamp(timestamp: Long) extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction contains an invalid schedule.
   */
  case class InvalidSchedule(schedule: Schedule) extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction contains an output with a non-positive quantity value.
   */
  case class NonPositiveOutputValue(outputValue: Value) extends TransactionSyntaxError

  /**
   * A Syntax error indicating that the inputs of this transaction cannot satisfy the outputs.
   */
  case class InsufficientInputFunds(inputs: List[Value.Value], outputs: List[Value.Value])
      extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction contains a proof whose type does not match its corresponding proposition.
   */
  case class InvalidProofType(proposition: Proposition, proof: Proof) extends TransactionSyntaxError

  /**
   * A Syntax error indicating that the size of this transaction is invalid.
   */
  case object InvalidDataLength extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction contains invalid UpdateProposals
   */
  case class InvalidUpdateProposal(outputs: Seq[Value.UpdateProposal]) extends TransactionSyntaxError

  /**
   * A Syntax error indicating that this transaction contains invalid MergingStatements
   */
  case class InvalidMergingStatements(statements: Seq[AssetMergingStatement]) extends TransactionSyntaxError

  /**
   * A Syntax error indicating that the request merging operation is invalid
   */
  case class IncompatibleMerge(inputs: Seq[Value.Asset], output: Value.Asset) extends TransactionSyntaxError
}
