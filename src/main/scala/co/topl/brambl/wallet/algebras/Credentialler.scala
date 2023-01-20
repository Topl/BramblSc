package co.topl.brambl.wallet.algebras

import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.{TransactionAuthorizationError, TransactionSyntaxError, ValidationError}
import co.topl.quivr.runtime.DynamicContext

trait Credentialler[F[_]] {
  def prove(unprovenTx: IoTransaction): F[Either[List[TransactionSyntaxError], IoTransaction]]
  def validate(tx: IoTransaction, ctx: DynamicContext[F, String, Datum]): F[List[TransactionAuthorizationError]]
  def proveAndValidate(
    unprovenTx: IoTransaction,
    ctx:        DynamicContext[F, String, Datum]
  ): F[Either[List[ValidationError], IoTransaction]]
}
