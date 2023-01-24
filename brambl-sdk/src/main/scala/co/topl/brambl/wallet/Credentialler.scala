package co.topl.brambl.wallet

import co.topl.brambl.Context
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.{TransactionAuthorizationError, TransactionSyntaxError, ValidationError}

trait Credentialler {
  def prove(unprovenTx: IoTransaction): Either[List[TransactionSyntaxError], IoTransaction]
  def validate(tx:      IoTransaction, ctx: Context): List[TransactionAuthorizationError]

  def proveAndValidate(
    unprovenTx: IoTransaction,
    ctx:        Context
  ): Either[List[ValidationError], IoTransaction]
}
