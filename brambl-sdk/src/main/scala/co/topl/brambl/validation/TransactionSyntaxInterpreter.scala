package co.topl.brambl.validation

import cats.Monad
import cats.implicits._
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances._
import quivr.models.Int128

object TransactionSyntaxInterpreter {

  final val MaxDataLength = 15360

  def make[F[_]: Monad](): TransactionSyntaxVerifier[F] = new TransactionSyntaxVerifier[F] {

    override def validate(t: IoTransaction): F[Either[TransactionSyntaxError, IoTransaction]] =
      validators
        .foldMap(_ apply t)
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
      scheduleValidation,
      positiveOutputValuesValidation,
      sufficientFundsValidation,
//      proofTypeValidation, // TODO: Add check for proof length
      dataLengthValidation
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
            case (Some(knownIdentifier), inputs) if inputs.size > 1 =>
              TransactionSyntaxError.DuplicateInput(knownIdentifier)
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
      transaction.datum.get.event.get.schedule.get.timestamp >= 0, // TODO: This should be replaced after PB validation is included
      (),
      TransactionSyntaxError.InvalidTimestamp(transaction.datum.get.event.get.schedule.get.timestamp)
    )

  /**
   * Verify that the schedule of the timestamp contains valid minimum and maximum slot values
   */
  private def scheduleValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    // TODO: This should be replaced after PB validation is included
    Validated.condNec(
      transaction.datum.get.event.get.schedule.get.max >= transaction.datum.get.event.get.schedule.get.min &&
      transaction.datum.get.event.get.schedule.get.min >= 0,
      (),
      TransactionSyntaxError.InvalidSchedule(transaction.datum.get.event.get.schedule.get)
    )

  /**
   * Verify that each transaction output contains a positive quantity (where applicable)
   */
  private def positiveOutputValuesValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    transaction.outputs
      .foldMap[ValidatedNec[TransactionSyntaxError, Unit]](output =>
        (output.value.get.value match {
          case Value.Value.Token(Value.Token(Some(Int128(q, _)), _))       => BigInt(q.toByteArray).some
          case Value.Value.Asset(Value.Asset(_, Some(Int128(q, _)), _, _)) => BigInt(q.toByteArray).some
          case _ => none // TODO: This will be unneeded when PB validation is included
        }).foldMap((quantity: BigInt) =>
          Validated
            .condNec(
              quantity > BigInt(0),
              (),
              // TODO: .get will be unneeded when PB validation is included
              TransactionSyntaxError.NonPositiveOutputValue(output.value.get): TransactionSyntaxError
            )
        )
      )

  /**
   * Ensure the input value quantities exceed or equal the (non-minting) output value quantities
   *
   * TODO: IoTransaction model currently does not support minting
   */
  private def sufficientFundsValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    quantityBasedValidation(transaction) { f =>
      // TODO: .get will be unneeded when PB validation is included
      val filteredInputs = transaction.inputs.map(_.value.get.value).filter(f.isDefinedAt)
      val filteredOutputs = transaction.outputs.map(_.value.get.value).filter(f.isDefinedAt)
      val inputsSum = filteredInputs.map(f).sumAll
      val outputsSum = filteredOutputs.map(f).sumAll
      Validated.condNec(
        inputsSum >= outputsSum,
        (),
        TransactionSyntaxError.InsufficientInputFunds(
          filteredInputs.toList,
          filteredOutputs.toList
        ): TransactionSyntaxError
      )
    }

  /**
   * Perform validation based on the quantities of boxes grouped by type
   *
   * @param f an extractor function which retrieves a BigInt from a Box.Value
   */
  private def quantityBasedValidation(transaction: IoTransaction)(
    f: PartialFunction[Value.Value, BigInt] => ValidatedNec[TransactionSyntaxError, Unit]
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    NonEmptyChain(
      // Extract all Token values and their quantities
      f { case Value.Value.Token(Value.Token(Some(Int128(q, _)), _)) => BigInt(q.toByteArray) },
      // Extract all Asset values and their quantities
      f { case Value.Value.Asset(Value.Asset(_, Some(Int128(q, _)), _, _)) => BigInt(q.toByteArray) }
    ).appendChain(
      // Extract all Asset values (grouped by asset label) and their quantities
      Chain.fromSeq(
        (transaction.inputs.map(_.value.get.value) ++ transaction.outputs.map(_.value.get.value))
          .collect { case Value.Value.Asset(Value.Asset(label, _, _, _)) => label }
          .toList
          .distinct
          .map(code =>
            f {
              case Value.Value.Asset(Value.Asset(label, Some(Int128(q, _)), _, _)) if label === code =>
                BigInt(q.toByteArray)
            }
          )
      )
    ).combineAll

  /**
   * DataLengthValidation validates approved transaction data length, includes proofs
   * TODO should we include proofs lengths?:
   *
   * @see [[https://topl.atlassian.net/browse/BN-708]]
   * @param transaction transaction
   * @return
   */
  private def dataLengthValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(
      transaction.immutable.value.size <= MaxDataLength,
      (),
      TransactionSyntaxError.InvalidDataLength
    )
}
