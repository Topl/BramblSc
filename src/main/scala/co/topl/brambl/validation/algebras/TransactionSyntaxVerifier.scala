package co.topl.brambl.validation.algebras

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.common.ContextlessValidation
import co.topl.brambl.validation.TransactionSyntaxError

trait TransactionSyntaxVerifier[F[_]] extends ContextlessValidation[F, TransactionSyntaxError, IoTransaction]
