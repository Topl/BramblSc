package co.topl.brambl.validation

import cats.Monad
import cats.implicits._
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier

object TransactionSyntaxInterpreter {
  def make[F[_]: Monad](): TransactionSyntaxVerifier[F] = new TransactionSyntaxVerifier[F] {
    override def validate(t: IoTransaction): F[Either[TransactionSyntaxError, IoTransaction]] =
      validators.foldMap(_ apply t)
        .toEither
        // TODO: The following line is a temporary measure until we decide on a return type
        .leftMap[TransactionSyntaxError](_ => TransactionSyntaxError.SyntaxFailed)
        .as(t)
        .pure[F]
  }

  private val validators: Chain[IoTransaction => ValidatedNec[TransactionSyntaxError, Unit]] =
    Chain(
      nonEmptyInputsValidation,
      distinctInputsValidation,
      maximumOutputsCountValidation,
      positiveTimestampValidation,
//      scheduleValidation,
//      positiveOutputValuesValidation,
//      sufficientFundsValidation,
//      proofTypeValidation,
//      dataLengthValidation
    )

  /**
   * Verify that this transaction contains at least one input
   */
  private def nonEmptyInputsValidation(
                                                      transaction: IoTransaction
                                                    ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(transaction.inputs.nonEmpty, (), TransactionSyntaxError.EmptyInputs)

  /**
   * Verify that this transaction does not spend the same box more than once
   */
  private def distinctInputsValidation(
                                                      transaction: IoTransaction
                                                    ): ValidatedNec[TransactionSyntaxError, Unit] =
    NonEmptyChain
      .fromSeq(
        transaction.inputs
          .groupBy(_.knownIdentifier)
          .collect {
            case (Some(knownIdentifier), inputs) if inputs.size > 1 => TransactionSyntaxError.DuplicateInput(knownIdentifier)
          }
          .toSeq
      )
      .fold(().validNec[TransactionSyntaxError])(_.invalid[Unit])

  /**
   * Verify that this transaction does not contain too many outputs.  A transaction's outputs are referenced by index,
   * but that index must be a Short value.
   */
  private def maximumOutputsCountValidation(
                                                           transaction: IoTransaction
                                                         ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(transaction.outputs.size < Short.MaxValue, (), TransactionSyntaxError.ExcessiveOutputsCount)

  /**
   * Verify that the timestamp of the transaction is positive (greater than 0).  Transactions _can_ be created
   * in the past.
   */
  private def positiveTimestampValidation(
                                                         transaction: IoTransaction
                                                       ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(
      transaction.datum.get.event.get.schedule.get.timestamp >= 0,
      (),
      TransactionSyntaxError.InvalidTimestamp(transaction.datum.get.event.get.schedule.get.timestamp)
    )
}
