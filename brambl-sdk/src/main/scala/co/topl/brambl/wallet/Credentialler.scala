package co.topl.brambl.wallet

import co.topl.brambl.Context
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.ValidationError
import quivr.models.Proof

/**
 * Defines a Credentialler. A Credentialler is responsible for proving and verifying transactions.
 */
trait Credentialler[F[_]] {

  /**
   * Prove a transaction. That is, prove all the inputs within the transaction if possible
   *
   * Note: If a proposition is unable to be proven, it's proof will be Proof.Value.Empty
   *
   * @param unprovenTx The unproven transaction to prove
   * @return The proven version of the transaction.
   */
  def prove(unprovenTx: IoTransaction): F[IoTransaction]

  /**
   * Validate whether the transaction is syntactically valid and authorized.
   * A Transaction is authorized if all contained attestations are satisfied
   *
   * TODO: Revisit when we have cost estimator to decide which validations should occur
   *
   * @param tx  Transaction to validate
   * @param ctx Context to validate the transaction in
   * @return List of validation errors, if any
   */
  def validate(tx: IoTransaction, ctx: Context[F]): F[List[ValidationError]]

  /**
   * Prove and validate a transaction.
   * That is, attempt to prove all the inputs within the transaction and then validate if the transaction
   * is syntactically valid and successfully proven
   *
   * @param unprovenTx The unproven transaction to prove
   * @return The proven version of the input if valid. Else the validation errors
   */
  def proveAndValidate(
    unprovenTx: IoTransaction,
    ctx:        Context[F]
  ): F[Either[List[ValidationError], IoTransaction]]
}
