package co.topl.brambl.generators

import co.topl.brambl.models.{Datum, Event, Evidence, Identifier, KnownIdentifier}
import co.topl.brambl.models.transaction.Schedule
import co.topl.quivr.generators.ModelGenerators.{arbitraryDigest32, arbitraryDigest64}
import com.google.protobuf.ByteString
import org.scalacheck.{Arbitrary, Gen}

trait TransactionGenerator {

  implicit val arbitrarySchedule: Arbitrary[Schedule] =
    Arbitrary(
      for {
        min       <- Gen.chooseNum(0L, 50L)
        max       <- Gen.chooseNum(0L, 50L)
        timestamp <- Gen.chooseNum(0L, 50L)
      } yield Schedule.of(min, max, timestamp)
    )
}

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

trait KnownIdentifierGenerator extends IdentifierGenerator {

  implicit val arbitraryTransactionOutput32: Arbitrary[KnownIdentifier.TransactionOutput32] =
    Arbitrary(
      for {
        network <- Gen.chooseNum(0, 50)
        ledger  <- Gen.chooseNum(0, 50)
        index   <- Gen.chooseNum(0, 50)
        id      <- arbitraryIoTransaction32.arbitrary
      } yield KnownIdentifier.TransactionOutput32.of(network, ledger, index, id)
    )

  implicit val arbitraryTransactionOutput64: Arbitrary[KnownIdentifier.TransactionOutput64] =
    Arbitrary(
      for {
        network <- Gen.chooseNum(0, 50)
        ledger  <- Gen.chooseNum(0, 50)
        index   <- Gen.chooseNum(0, 50)
        id      <- arbitraryIoTransaction64.arbitrary
      } yield KnownIdentifier.TransactionOutput64.of(network, ledger, index, id)
    )

}

trait EventGenerator extends TransactionGenerator with KnownIdentifierGenerator {

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
        references32 <- Gen
          .containerOfN[Seq, KnownIdentifier.TransactionOutput32](3, arbitraryTransactionOutput32.arbitrary)
        references64 <- Gen
          .containerOfN[Seq, KnownIdentifier.TransactionOutput64](3, arbitraryTransactionOutput64.arbitrary)
        metadata <- Gen.const(
          quivr.models.SmallData.of(ByteString.EMPTY)
        ) // TODO create Small Data generator: QuivrRepo
      } yield Event.IoTransaction.of(schedule, references32, references64, metadata)
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

trait ModelGenerators extends DatumGenerator with KnownIdentifierGenerator with EvidenceGenerator with EventGenerator

object ModelGenerators extends ModelGenerators
