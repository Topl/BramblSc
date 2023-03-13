package co.topl.brambl.builders

import cats._
import cats.data.EitherT
import cats.data.Validated
import cats.implicits._
import cats.implicits.catsSyntaxApplicativeId
import cats.implicits.toFlatMapOps
import co.topl.brambl.dataApi.DataApi
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.builders.InputBuildRequest
import co.topl.brambl.models.builders.OutputBuildRequest
import co.topl.brambl.models.box.Attestation
import co.topl.brambl.models.transaction._
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event
import quivr.models.Proof
import quivr.models.SmallData

object TransactionBuilderInterpreter {

  def make[F[_]: Monad](dataApi: DataApi): TransactionBuilder[F] = new TransactionBuilder[F] {

    override def constructUnprovenInput(
      data: InputBuildRequest
    ): F[Either[BuilderError.InputBuilderError, SpentTransactionOutput]] = {
      val box = dataApi.getBoxByKnownIdentifier(data.address)
      val attestation = box.map(_.lock).map(constructUnprovenAttestation)
      val value = box.map(_.value)
      (attestation, value) match {
        case (Some(Right(att)), Some(boxVal)) =>
          SpentTransactionOutput(data.address, att, boxVal)
            .asRight[BuilderError.InputBuilderError]
            .pure[F]
        case (Some(Left(err)), _) => err.asLeft[SpentTransactionOutput].pure[F]
        case _ =>
          BuilderError
            .InputBuilderError(
              s"Could not construct input. Id=${data.address}, Attestation=${attestation}, Value=${value}"
            )
            .asLeft[SpentTransactionOutput]
            .pure[F]
      }
    }

    override def constructOutput(
      data: OutputBuildRequest
    ): F[Either[BuilderError.OutputBuilderError, UnspentTransactionOutput]] =
      UnspentTransactionOutput(data.address, data.value)
        .asRight[BuilderError.OutputBuilderError]
        .pure[F]

    override def constructUnprovenTransaction(
      inputRequests:  List[InputBuildRequest],
      outputRequests: List[OutputBuildRequest],
      schedule:       Option[Schedule],
      outputRefs:     List[TransactionOutputAddress],
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
            Event.IoTransaction(
              // TODO: Replace min and max with slot numbers
              schedule.getOrElse(Schedule(0, 2147483647, System.currentTimeMillis)),
              metadata.getOrElse(SmallData())
            )
          )
          IoTransaction(i, o, datum).asRight[List[BuilderError]]
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
                p,
                List.fill(p.challenges.length)(Proof()) // Its unproven
              )
            )
          )
        case _ => Left(BuilderError.InputBuilderError("Only considering Predicate locks for now"))
      }
  }
}
