package co.topl.brambl.builders

import cats._
import cats.implicits._
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEitherObject, catsSyntaxOptionId, toFlatMapOps}
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.dataApi.DataApi
import co.topl.brambl.models.{Address, Datum, Event, Identifier, KnownIdentifier}
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.builders.{InputBuildRequest, OutputBuildRequest}
import co.topl.brambl.models.transaction.{
  Attestation,
  IoTransaction,
  Schedule,
  SpentTransactionOutput,
  UnspentTransactionOutput
}
import quivr.models.{Proof, SmallData}
import cats.data.{EitherT, Validated}

object TransactionBuilderInterpreter {

  def make[F[_]: Monad](dataApi: DataApi): TransactionBuilder[F] = new TransactionBuilder[F] {

    override def constructUnprovenInput(
      data: InputBuildRequest
    ): F[Either[BuilderError.InputBuilderError, SpentTransactionOutput]] = {
      val id = data.id
      val box = id.flatMap(dataApi.getBoxByKnownIdentifier)
      val attestation = box.flatMap(_.lock).map(constructUnprovenAttestation)
      val value = box.flatMap(_.value)
      val datum = Datum.SpentOutput(
        Event
          .SpentTransactionOutput(
            if (data.metadata.isDefined) data.metadata else EmptyData.some
          )
          .some
      )
      val opts = List()
      (id, attestation, value) match {
        case (Some(knownId), Some(Right(att)), Some(boxVal)) =>
          SpentTransactionOutput(knownId.some, att.some, boxVal.some, datum.some, opts)
            .asRight[BuilderError.InputBuilderError]
            .pure[F]
        case (_, Some(Left(err)), _) => err.asLeft[SpentTransactionOutput].pure[F]
        case _ =>
          BuilderError
            .InputBuilderError(
              s"Could not construct input. Id=${id}, Attestation=${attestation}, Value=${value}"
            )
            .asLeft[SpentTransactionOutput]
            .pure[F]
      }
    }

    override def constructOutput(
      data: OutputBuildRequest
    ): F[Either[BuilderError.OutputBuilderError, UnspentTransactionOutput]] = {
      // TODO: Replace with non-hardcoded values
      val Network = 0
      val Ledger = 0

      (data.lock, data.value) match {
        case (Some(lock), Some(value)) =>
          val address =
            Address(Network, Ledger, Identifier().withLock32(Identifier.Lock32(lock.sized32Evidence.some)).some)
          val datum = Datum.UnspentOutput(
            Event
              .UnspentTransactionOutput(
                if (data.metadata.isDefined) data.metadata else EmptyData.some
              )
              .some
          )
          UnspentTransactionOutput(address.some, value.some, datum.some)
            .asRight[BuilderError.OutputBuilderError]
            .pure[F]
        case _ =>
          BuilderError
            .OutputBuilderError(s"Could not construct output. Lock=${data.lock}, Value=${data.value}")
            .asLeft[UnspentTransactionOutput]
            .pure[F]
      }
    }

    override def constructUnprovenTransaction(
      inputRequests:  List[InputBuildRequest],
      outputRequests: List[OutputBuildRequest],
      schedule:       Option[Schedule],
      output32Refs:   List[KnownIdentifier.TransactionOutput32],
      output64Refs:   List[KnownIdentifier.TransactionOutput64],
      metadata:       Option[SmallData]
    ): F[Either[List[BuilderError], IoTransaction]] = {
      // Build the inputs
      val inputs = inputRequests
        .map(constructUnprovenInput) // List[F[Either]]
        // Convert to ValidatedNel to retain all the errors
        .map(EitherT(_).toValidatedNel) // List[F[ValidatedNel[Error, Output]]]
        // Only works if F is an Applicative (Monad and Validated are)
        .sequence // List[F[ValidatedNel[Error, Output]]] => F[List[ValidatedNel[Error, Output]]]
        .map(_.sequence) // F[List[ValidatedNel[Error, Output]]] => F[Validated[Nel[Error], List[Output]]]
      // Build the outputs
      val outputs = outputRequests
        .map(constructOutput)
        .map(EitherT(_).toValidatedNel)
        .sequence
        .map(_.sequence)

      for {
        ins  <- inputs
        outs <- outputs
      } yield (ins, outs) match {
        case (Validated.Valid(i), Validated.Valid(o)) =>
          val datum = Datum.IoTransaction(
            Event
              .IoTransaction(
                if (schedule.isDefined) schedule
                else
                  Schedule(0, 2147483647, System.currentTimeMillis).some, // TODO: Replace min and max with slot numbers
                output32Refs,
                output64Refs,
                if (metadata.isDefined) metadata else EmptyData.some
              )
              .some
          )
          IoTransaction(i, o, datum.some).asRight[List[BuilderError]]
        case (Validated.Invalid(errsI), Validated.Invalid(errsO)) =>
          (errsI.toList ++ errsO.toList).asLeft[IoTransaction]
        case (Validated.Invalid(errs), _) => errs.toList.asLeft[IoTransaction]
        case (_, Validated.Invalid(errs)) => errs.toList.asLeft[IoTransaction]
      }
    }

    /**
     * Construct an unproven attestation for a given lock
     *
     * @param lock The lock for which we are building the attestation
     * @return Either an InputBuilderError or the built unproven attestation
     */
    private def constructUnprovenAttestation(lock: Lock): Either[BuilderError.InputBuilderError, Attestation] =
      lock.value match {
        case Lock.Value.Predicate(p) =>
          Right(
            Attestation().withPredicate(
              Attestation.Predicate(
                p.some,
                List.fill(p.challenges.length)(Proof()) // Its unproven
              )
            )
          )
        case _ => Left(BuilderError.InputBuilderError("Only considering Predicate locks for now"))
      }
  }
}
