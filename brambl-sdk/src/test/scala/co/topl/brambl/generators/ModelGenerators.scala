package co.topl.brambl.generators

import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.Attestation
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{Datum, Event, Evidence, Identifier}
import co.topl.brambl.models.transaction.Schedule
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.quivr.generators.ModelGenerators.{arbitraryDigest32, arbitraryDigest64}
import com.google.protobuf.ByteString
import org.scalacheck.{Arbitrary, Gen}
import quivr.models.Int128
import quivr.models.Proof

import java.util.Random

trait EvidenceGenerator {

  implicit val arbitraryEvidenceSized32: Arbitrary[Evidence.Sized32] =
    Arbitrary(
      for {
        digest <- arbitraryDigest32.arbitrary
      } yield Evidence.Sized32.of(digest)
    )

  implicit val arbitraryEvidenceSized64: Arbitrary[Evidence.Sized64] =
    Arbitrary(
      for {
        digest <- arbitraryDigest64.arbitrary
      } yield Evidence.Sized64.of(digest)
    )
}

trait IdentifierGenerator extends EvidenceGenerator {

  implicit val arbitraryLock32: Arbitrary[Identifier.Lock32] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized32.arbitrary
      } yield Identifier.Lock32.of(evidence)
    )

  implicit val arbitraryLock64: Arbitrary[Identifier.Lock64] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized64.arbitrary
      } yield Identifier.Lock64.of(evidence)
    )

  implicit val arbitraryBoxValue32: Arbitrary[Identifier.BoxValue32] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized32.arbitrary
      } yield Identifier.BoxValue32.of(evidence)
    )

  implicit val arbitraryBoxValue64: Arbitrary[Identifier.BoxValue64] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized64.arbitrary
      } yield Identifier.BoxValue64.of(evidence)
    )

  implicit val arbitraryIoTransaction32: Arbitrary[Identifier.IoTransaction32] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized32.arbitrary
      } yield Identifier.IoTransaction32.of(evidence)
    )

  implicit val arbitraryIoTransaction64: Arbitrary[Identifier.IoTransaction64] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized64.arbitrary
      } yield Identifier.IoTransaction64.of(evidence)
    )

  implicit val arbitraryAccumulatorRoot32: Arbitrary[Identifier.AccumulatorRoot32] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized32.arbitrary
      } yield Identifier.AccumulatorRoot32.of(evidence)
    )

  implicit val arbitraryAccumulatorRoot64: Arbitrary[Identifier.AccumulatorRoot64] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized64.arbitrary
      } yield Identifier.AccumulatorRoot64.of(evidence)
    )
}

trait LockAddressGenerator extends IdentifierGenerator {

  implicit val arbitraryLockAddress: Arbitrary[LockAddress] =
    Arbitrary(
      for {
        network <- Gen.chooseNum(0, 50)
        ledger  <- Gen.chooseNum(0, 50)
        id      <- arbitraryLock32.arbitrary
      } yield LockAddress(network, ledger, LockAddress.Id.Lock32(id))
    )
}

trait TransactionOutputAddressGenerator extends IdentifierGenerator {

  implicit val arbitraryTransactionOutputAddress: Arbitrary[TransactionOutputAddress] =
    Arbitrary(
      for {
        network <- Gen.chooseNum(0, 50)
        ledger  <- Gen.chooseNum(0, 50)
        index   <- Gen.chooseNum(0, 50)
        id      <- arbitraryIoTransaction32.arbitrary
      } yield TransactionOutputAddress(network, ledger, index, TransactionOutputAddress.Id.IoTransaction32(id))
    )

}

trait LockGenerator {

  implicit val arbitraryPredicateLock: Arbitrary[Lock.Predicate] =
    Arbitrary(
      Gen.const(
        Lock.Predicate.defaultInstance // TODO
      )
    )
}

trait ProofGenerator {

  implicit val arbitraryProof: Arbitrary[Proof] =
    Arbitrary(
      Gen.const(
        Proof.defaultInstance // TODO
      )
    )
}

trait AttestationGenerator extends LockGenerator with ProofGenerator {

  implicit val arbitraryAttestation: Arbitrary[Attestation] =
    Arbitrary(
      for {
        lock      <- arbitraryPredicateLock.arbitrary
        responses <- Gen.listOf(arbitraryProof.arbitrary)
      } yield Attestation().withPredicate(Attestation.Predicate(lock, responses))
    )
}

trait EventGenerator extends TransactionOutputAddressGenerator {

  implicit val arbitrarySchedule: Arbitrary[Schedule] =
    Arbitrary(
      for {
        min       <- Gen.chooseNum(0L, 50L)
        max       <- Gen.chooseNum(0L, 50L)
        timestamp <- Gen.chooseNum(0L, 50L)
      } yield Schedule.of(min, min + max, timestamp)
    )

  implicit val arbitraryEventEon: Arbitrary[Event.Eon] =
    Arbitrary(
      for {
        beginSlot <- Gen.chooseNum(0L, 50L)
        height    <- Gen.chooseNum(0L, 50L)
      } yield Event.Eon.of(beginSlot, height)
    )

  implicit val arbitraryEventEra: Arbitrary[Event.Era] =
    Arbitrary(
      for {
        beginSlot <- Gen.chooseNum(0L, 50L)
        height    <- Gen.chooseNum(0L, 50L)
      } yield Event.Era.of(beginSlot, height)
    )

  implicit val arbitraryEventEpoch: Arbitrary[Event.Epoch] =
    Arbitrary(
      for {
        beginSlot <- Gen.chooseNum(0L, 50L)
        height    <- Gen.chooseNum(0L, 50L)
      } yield Event.Epoch.of(beginSlot, height)
    )

  implicit val arbitraryEventHeader: Arbitrary[Event.Header] =
    Arbitrary(
      for {
        height <- Gen.chooseNum(0L, 50L)
      } yield Event.Header.of(height)
    )

  implicit val arbitraryEventIoTransaction: Arbitrary[Event.IoTransaction] =
    Arbitrary(
      for {
        schedule <- arbitrarySchedule.arbitrary
        metadata <- Gen.const(
          quivr.models.SmallData.of(ByteString.EMPTY)
        ) // TODO create Small Data generator: QuivrRepo
      } yield Event.IoTransaction.of(schedule, metadata)
    )

  implicit val arbitraryEventSpentTransactionOutput: Arbitrary[Event.SpentTransactionOutput] =
    Arbitrary(
      for {
        metadata <- Gen.const(
          quivr.models.SmallData.of(ByteString.EMPTY)
        ) // TODO create Small Data generator: QuivrRepo
      } yield Event.SpentTransactionOutput.of(metadata)
    )

  implicit val arbitraryEventUnspentTransactionOutput: Arbitrary[Event.UnspentTransactionOutput] =
    Arbitrary(
      for {
        metadata <- Gen.const(
          quivr.models.SmallData.of(ByteString.EMPTY)
        ) // TODO create Small Data generator: QuivrRepo
      } yield Event.UnspentTransactionOutput.of(metadata)
    )

  implicit val arbitraryEventRoot: Arbitrary[Event.Root] =
    Arbitrary(
      for {
        value <- Gen.const(
          quivr.models.Root.of(quivr.models.Root.Value.Empty)
        ) // TODO create Root generator: QuivrRepo
      } yield Event.Root.of(value)
    )

}

trait DatumGenerator extends EventGenerator {

  implicit val genDatumEon: Gen[Datum.Eon] =
    implicitly[Arbitrary[Event.Eon]].arbitrary.map(Datum.Eon.of)

  implicit val genDatumEra: Gen[Datum.Era] =
    implicitly[Arbitrary[Event.Era]].arbitrary.map(Datum.Era.of)

  implicit val genDatumEpoch: Gen[Datum.Epoch] =
    implicitly[Arbitrary[Event.Epoch]].arbitrary.map(Datum.Epoch.of)

  implicit val genDatumHeader: Gen[Datum.Header] =
    implicitly[Arbitrary[Event.Header]].arbitrary.map(Datum.Header.of)

  implicit val genDatumRoot: Gen[Datum.Root] =
    implicitly[Arbitrary[Event.Root]].arbitrary.map(Datum.Root.of)

  implicit val genDatumIoTransaction: Gen[Datum.IoTransaction] =
    implicitly[Arbitrary[Event.IoTransaction]].arbitrary.map(Datum.IoTransaction.of)

  implicit val genDatumSpentOutput: Gen[Datum.SpentOutput] =
    implicitly[Arbitrary[Event.SpentTransactionOutput]].arbitrary.map(Datum.SpentOutput.of)

  implicit val genDatumUnspentOutput: Gen[Datum.UnspentOutput] =
    implicitly[Arbitrary[Event.UnspentTransactionOutput]].arbitrary.map(Datum.UnspentOutput.of)

}

trait ValueGenerator {

  implicit val arbitraryInt128: Arbitrary[Int128] =
    Arbitrary(
      Gen.uuid
        .map(_ => BigInt(127, new Random()))
        .map(_.toByteArray)
        .map(ByteString.copyFrom)
        .map(Int128(_))
    )

  implicit val arbitraryValue: Arbitrary[Value] =
    Arbitrary(
      for {
        quantity <- arbitraryInt128.arbitrary
      } yield Value(Value.Value.Lvl(Value.LVL(quantity)))
    )
}

trait TransactionGenerator
    extends DatumGenerator
    with LockAddressGenerator
    with ValueGenerator
    with TransactionOutputAddressGenerator
    with AttestationGenerator {

  implicit val arbitraryUnspentTransactionOutput: Arbitrary[UnspentTransactionOutput] =
    Arbitrary(
      for {
        lockAddress <- arbitraryLockAddress.arbitrary
        value       <- arbitraryValue.arbitrary
      } yield UnspentTransactionOutput(lockAddress, value)
    )

  implicit val arbitrarySpentTransactionOutput: Arbitrary[SpentTransactionOutput] =
    Arbitrary(
      for {
        address     <- arbitraryTransactionOutputAddress.arbitrary
        attestation <- arbitraryAttestation.arbitrary
        value       <- arbitraryValue.arbitrary
      } yield SpentTransactionOutput(address, attestation, value)
    )

  implicit val arbitraryIoTransaction: Arbitrary[IoTransaction] =
    Arbitrary(
      for {
        inputs  <- Gen.listOf(arbitrarySpentTransactionOutput.arbitrary)
        outputs <- Gen.listOf(arbitraryUnspentTransactionOutput.arbitrary)
        datum   <- genDatumIoTransaction
      } yield IoTransaction(inputs, outputs, datum)
    ) // TODO
}

trait ModelGenerators
    extends DatumGenerator
    with TransactionOutputAddressGenerator
    with EvidenceGenerator
    with EventGenerator
    with TransactionGenerator

object ModelGenerators extends ModelGenerators
