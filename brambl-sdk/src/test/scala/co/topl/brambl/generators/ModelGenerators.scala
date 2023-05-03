package co.topl.brambl.generators

import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction._
import co.topl.brambl.models._
import co.topl.quivr.generators.ModelGenerators.arbitraryDigest
import com.google.protobuf.ByteString
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import quivr.models.Int128
import quivr.models.Proof

import java.util.Random

trait EvidenceGenerator {

  implicit val arbitraryEvidenceSized: Arbitrary[Evidence] =
    Arbitrary(
      for {
        digest <- arbitraryDigest.arbitrary
      } yield Evidence(digest)
    )
}

trait IdentifierGenerator extends EvidenceGenerator {

  implicit val arbitraryLockId: Arbitrary[LockId] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized.arbitrary
      } yield LockId(evidence.digest.value)
    )

  implicit val arbitraryTransactionId: Arbitrary[TransactionId] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized.arbitrary
      } yield TransactionId(evidence.digest.value)
    )

  implicit val arbitraryAccumulatorRootId: Arbitrary[AccumulatorRootId] =
    Arbitrary(
      for {
        evidence <- arbitraryEvidenceSized.arbitrary
      } yield AccumulatorRootId(evidence.digest.value)
    )
}

trait LockAddressGenerator extends IdentifierGenerator {

  implicit val arbitraryLockAddress: Arbitrary[LockAddress] =
    Arbitrary(
      for {
        network <- Gen.chooseNum(0, 50)
        ledger  <- Gen.chooseNum(0, 50)
        id      <- arbitraryLockId.arbitrary
      } yield LockAddress(network, ledger, id)
    )
}

trait TransactionOutputAddressGenerator extends IdentifierGenerator {

  implicit val arbitraryTransactionOutputAddress: Arbitrary[TransactionOutputAddress] =
    Arbitrary(
      for {
        network <- Gen.chooseNum(0, 50)
        ledger  <- Gen.chooseNum(0, 50)
        index   <- Gen.chooseNum(0, 50)
        id      <- arbitraryTransactionId.arbitrary
      } yield TransactionOutputAddress(network, ledger, index, id)
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

trait EventGenerator {

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

  implicit val genDatumIoTransaction: Gen[Datum.IoTransaction] =
    implicitly[Arbitrary[Event.IoTransaction]].arbitrary.map(Datum.IoTransaction.of)

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
      } yield IoTransaction.defaultInstance.withInputs(inputs).withOutputs(outputs).withDatum(datum)
    ) // TODO
}

trait ModelGenerators
    extends DatumGenerator
    with TransactionOutputAddressGenerator
    with EvidenceGenerator
    with EventGenerator
    with TransactionGenerator

object ModelGenerators extends ModelGenerators
