package co.topl.brambl.builders

import cats.implicits._
import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.dataApi.{DataApi, MockDataApi}
import co.topl.brambl.models.KnownIdentifier.{TransactionOutput32, TransactionOutput64}
import co.topl.brambl.models.{Address, Datum, Event, Identifier}
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.builders.{InputBuildRequest, OutputBuildRequest}
import co.topl.brambl.models.transaction.{Attestation, IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import quivr.models.{Proof, SmallData}
import co.topl.brambl.typeclasses.ContainsEvidence._
import co.topl.brambl.typeclasses.ContainsSignable.instances.lockSignable
import com.google.protobuf.ByteString

/**
 * A builder for IoTransactions.
 */
object MockTransactionBuilder extends TransactionBuilder {
  override def constructUnprovenTransaction(
                                             inputRequests: List[InputBuildRequest],
                                             outputRequests: List[OutputBuildRequest],
                                             schedule: Option[Schedule] = None,
                                             output32Refs: List[TransactionOutput32] = List(),
                                             output64Refs: List[TransactionOutput64] = List(),
                                             metadata: Option[SmallData] = None
                                           ): Either[List[BuilderError], IoTransaction] = {
    val inputs = inputRequests
      .map(MockInputBuilder.constructUnprovenInput)
      .partitionMap[BuilderError, SpentTransactionOutput](identity)
    val outputs = outputRequests
      .map(MockOutputBuilder.constructOutput)
      .partitionMap[BuilderError, UnspentTransactionOutput](identity)
    if (inputs._1.isEmpty && outputs._1.isEmpty) {
      val datum = Datum.IoTransaction(Event.IoTransaction(
        if (schedule.isDefined) schedule
        else Schedule(0, 2147483647, System.currentTimeMillis).some, // TODO: Replace min and max with slot numbers
        output32Refs,
        output64Refs,
        if (metadata.isDefined) metadata else EmptyData.some
      ).some)
      Right(IoTransaction(inputs._2, outputs._2, datum.some))
    } else
      Left(inputs._1 ++ outputs._1)
  }

  override def constructOutput(data: OutputBuildRequest): Either[BuilderError.OutputBuilderError, UnspentTransactionOutput] = {
    // TODO: Replace with non-hardcoded values
    val Network = 0
    val Ledger = 0

    (data.lock, data.value) match {
      case (Some(lock), Some(value)) => {
        val address = Address(Network, Ledger,
          Identifier().withLock32(Identifier.Lock32(lock.sized32Evidence.some)).some
        )
        val datum = Datum.UnspentOutput(Event.UnspentTransactionOutput(
          if (data.metadata.isDefined) data.metadata else EmptyData.some
        ).some)
        Right(UnspentTransactionOutput(address.some, value.some, datum.some))
      }
      case _ => Left(BuilderError.OutputBuilderError(s"Could not construct output. Lock=${data.lock}, Value=${data.value}"))
    }
  }

  override def constructUnprovenInput(data: InputBuildRequest): Either[BuilderError.InputBuilderError, SpentTransactionOutput] = {
    val id = data.id
    val box = id.flatMap(MockDataApi.getBoxByKnownIdentifier)
    val attestation = box.flatMap(_.lock).map(constructUnprovenAttestation)
    val value = box.flatMap(_.value)
    val datum = Datum.SpentOutput(Event.SpentTransactionOutput(
      if (data.metadata.isDefined) data.metadata else EmptyData.some
    ).some)
    val opts = List()
    (id, attestation, value) match {
      case (Some(knownId), Some(Right(att)), Some(boxVal)) =>
        Right(SpentTransactionOutput(knownId.some, att.some, boxVal.some, datum.some, opts))
      case (_, Some(Left(err)), _) => Left(err)
      case _ =>
        Left(BuilderError.InputBuilderError(s"Could not construct input. Id=${id}, Attestation=${attestation}, Value=${value}"))
    }
  }

  /**
   * Construct an unproven attestation for a given lock
   *
   * @param lock The lock for which we are building the attestation
   * @return Either an InputBuilderError or the built unproven attestation
   */
  private def constructUnprovenAttestation(lock: Lock): Either[BuilderError.InputBuilderError, Attestation] = lock.value match {
    case Lock.Value.Predicate(p) => Right(
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
