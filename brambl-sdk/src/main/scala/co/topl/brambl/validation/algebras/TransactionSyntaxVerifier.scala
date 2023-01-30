package co.topl.brambl.validation.algebras

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.common.ContextlessValidation
import co.topl.brambl.validation.TransactionSyntaxError

// TODO: A decision should be made about what the return type of `validate` should be.
//  ValidatedNec? ValidatedNel? Either[List[Errors], Tx]?
trait TransactionSyntaxVerifier[F[_]] extends ContextlessValidation[F, TransactionSyntaxError, IoTransaction]
