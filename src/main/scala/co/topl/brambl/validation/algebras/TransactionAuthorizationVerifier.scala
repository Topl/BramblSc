package co.topl.brambl.validation.algebras

import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.common.ContextualValidation
import co.topl.quivr.runtime.DynamicContext
import co.topl.brambl.validation.TransactionAuthorizationError

trait TransactionAuthorizationVerifier[F[_]]
  extends ContextualValidation[F, TransactionAuthorizationError, IoTransaction, DynamicContext[F, String, Datum]]
