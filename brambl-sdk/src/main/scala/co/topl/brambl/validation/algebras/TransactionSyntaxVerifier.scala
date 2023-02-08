package co.topl.brambl.validation.algebras

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.common.ContextlessValidation
import co.topl.brambl.validation.TransactionSyntaxError
import cats.data.NonEmptyChain

trait TransactionSyntaxVerifier[F[_]]
    extends ContextlessValidation[F, NonEmptyChain[TransactionSyntaxError], IoTransaction]
