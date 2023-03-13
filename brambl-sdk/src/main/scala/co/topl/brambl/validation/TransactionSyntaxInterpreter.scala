package co.topl.brambl.validation

import cats.Applicative
import cats.implicits._
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import co.topl.brambl.models.box.{Lock, Value}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.models.box.Attestation
import quivr.models.{Int128, Proof, Proposition}

object TransactionSyntaxInterpreter {

  final val MaxDataLength = 15360

  def make[F[_]: Applicative](): TransactionSyntaxVerifier[F] = new TransactionSyntaxVerifier[F] {

    override def validate(t: IoTransaction): F[Either[NonEmptyChain[TransactionSyntaxError], IoTransaction]] =
      validators
        .foldMap(_ apply t)
        .toEither
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
      attestationValidation,
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
          .groupBy(_.address)
          .collect {
            case (knownIdentifier, inputs) if inputs.size > 1 =>
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
      transaction.datum.event.schedule.timestamp >= 0,
      (),
      TransactionSyntaxError.InvalidTimestamp(transaction.datum.event.schedule.timestamp)
    )

  /**
   * Verify that the schedule of the timestamp contains valid minimum and maximum slot values
   */
  private def scheduleValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(
      transaction.datum.event.schedule.max >= transaction.datum.event.schedule.min &&
      transaction.datum.event.schedule.min >= 0,
      (),
      TransactionSyntaxError.InvalidSchedule(transaction.datum.event.schedule)
    )

  /**
   * Verify that each transaction output contains a positive quantity (where applicable)
   */
  private def positiveOutputValuesValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    transaction.outputs
      .foldMap[ValidatedNec[TransactionSyntaxError, Unit]](output =>
        (output.value.value match {
          case Value.Value.Lvl(v)                                    => BigInt(v.quantity.value.toByteArray).some
          case Value.Value.Topl(v)                                   => BigInt(v.quantity.value.toByteArray).some
          case Value.Value.Asset(Value.Asset(_, Int128(q, _), _, _)) => BigInt(q.toByteArray).some
          case _                                                     => none
        }).foldMap((quantity: BigInt) =>
          Validated
            .condNec(
              quantity > BigInt(0),
              (),
              TransactionSyntaxError.NonPositiveOutputValue(output.value): TransactionSyntaxError
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
      val filteredInputs = transaction.inputs.map(_.value.value).filter(f.isDefinedAt)
      val filteredOutputs = transaction.outputs.map(_.value.value).filter(f.isDefinedAt)
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
      f { case Value.Value.Lvl(v) => BigInt(v.quantity.value.toByteArray) },
      f { case Value.Value.Topl(v) => BigInt(v.quantity.value.toByteArray) },
      // Extract all Asset values and their quantities
      f { case Value.Value.Asset(Value.Asset(_, Int128(q, _), _, _)) => BigInt(q.toByteArray) }
    ).appendChain(
      // Extract all Asset values (grouped by asset label) and their quantities
      Chain.fromSeq(
        (transaction.inputs.map(_.value.value) ++ transaction.outputs.map(_.value.value))
          .collect { case Value.Value.Asset(Value.Asset(label, _, _, _)) => label }
          .toList
          .distinct
          .map(code =>
            f {
              case Value.Value.Asset(Value.Asset(label, Int128(q, _), _, _)) if label === code =>
                BigInt(q.toByteArray)
            }
          )
      )
    ).combineAll

  /**
   * Validates that the attestations for each of the transaction's inputs are valid
   */
  private def attestationValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    transaction.inputs
      .map(input =>
        input.attestation.value match {
          case Attestation.Value.Predicate(Attestation.Predicate(lock, responses, _)) =>
            predicateLockProofTypeValidation(lock, responses)
          // TODO: There is no validation for Attestation types other than Predicate for now
          case _ => ().validNec[TransactionSyntaxError]
        }
      )
      .combineAll

  /**
   * Validates that the proofs associated with each proposition matches the expected _type_ for a Predicate Attestation
   *
   * (i.e. a DigitalSignature Proof that is associated with a HeightRange Proposition, this validation will fail)
   *
   * Preconditions: lock.challenges.length <= responses.length
   */
  private def predicateLockProofTypeValidation(
    lock:      Lock.Predicate,
    responses: Seq[Proof]
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    (lock.challenges zip responses)
      // TODO: Fix `.getRevealed`
      .map(challenge => proofTypeMatch(challenge._1.getRevealed, challenge._2))
      .combineAll

  /**
   * Validate that the type of Proof matches the type of the given Proposition
   * A Proof.Value.Empty type is considered valid for all Proposition types
   */
  private def proofTypeMatch(proposition: Proposition, proof: Proof): ValidatedNec[TransactionSyntaxError, Unit] =
    (proposition.value, proof.value) match {
      case (_, Proof.Value.Empty) =>
        ().validNec[TransactionSyntaxError] // Empty proofs are valid for all Proposition types
      case (Proposition.Value.Locked(_), Proof.Value.Locked(_)) => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.Digest(_), Proof.Value.Digest(_)) => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.DigitalSignature(_), Proof.Value.DigitalSignature(_)) =>
        ().validNec[TransactionSyntaxError]
      case (Proposition.Value.HeightRange(_), Proof.Value.HeightRange(_)) => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.TickRange(_), Proof.Value.TickRange(_))     => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.ExactMatch(_), Proof.Value.ExactMatch(_))   => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.LessThan(_), Proof.Value.LessThan(_))       => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.GreaterThan(_), Proof.Value.GreaterThan(_)) => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.EqualTo(_), Proof.Value.EqualTo(_))         => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.Threshold(_), Proof.Value.Threshold(_))     => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.Not(_), Proof.Value.Not(_))                 => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.And(_), Proof.Value.And(_))                 => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.Or(_), Proof.Value.Or(_))                   => ().validNec[TransactionSyntaxError]
      case _ => TransactionSyntaxError.InvalidProofType(proposition, proof).invalidNec[Unit]
    }

  /**
   * DataLengthValidation validates approved transaction data length, includes proofs
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
