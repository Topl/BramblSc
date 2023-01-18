package co.topl.brambl.wallet

import cats.Id
import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.ValidationError
import co.topl.brambl.validation.TransactionAuthorizationError
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.quivr.runtime.DynamicContext

trait Credentialler {
  def prove(unprovenTx: IoTransaction): Either[List[TransactionSyntaxError], IoTransaction]
  def validate(tx:      IoTransaction, ctx: DynamicContext[Id, String, Datum]): List[TransactionAuthorizationError]

  def proveAndValidate(
    unprovenTx: IoTransaction,
    ctx:        DynamicContext[Id, String, Datum]
  ): Either[List[ValidationError], IoTransaction]
}
