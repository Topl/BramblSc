package co.topl.brambl.generators

import co.topl.brambl.models.{Evidence, Identifier}
import co.topl.quivr.generators.ModelGenerators.{arbitraryDigest32, arbitraryDigest64}
import org.scalacheck.Arbitrary

trait ModelGenerators {

  implicit val arbitraryEvidence32: Arbitrary[Evidence.Sized32] =
    Arbitrary(
      for {
        d32 <- arbitraryDigest32.arbitrary
      } yield Evidence.Sized32.of(d32)
    )

  implicit val arbitraryEvidence64: Arbitrary[Evidence.Sized64] =
    Arbitrary(
      for {
        d64 <- arbitraryDigest64.arbitrary
      } yield Evidence.Sized64.of(d64)
    )

  implicit val arbitraryIoTransaction32: Arbitrary[Identifier.IoTransaction32] =
    Arbitrary(
      for {
        ev <- arbitraryEvidence32.arbitrary
      } yield Identifier.IoTransaction32.of(ev)
    )

  implicit val arbitraryIoTransaction64: Arbitrary[Identifier.IoTransaction64] =
    Arbitrary(
      for {
        ev <- arbitraryEvidence64.arbitrary
      } yield Identifier.IoTransaction64.of(ev)
    )
}

object ModelGenerators extends ModelGenerators
