package co.topl.brambl.generators

import co.topl.brambl.models.{Evidence, Identifier}
import com.google.protobuf.ByteString
import org.scalacheck.{Arbitrary, Gen}
import quivr.models.Digest.{Digest32, Digest64}

trait ModelGenerators {

  def genSizedStrictByteString(n: Int)(
    byteGen:                      Gen[Byte] = Gen.choose[Byte](0, 32)
  ): Gen[ByteString] =
    Gen
      .containerOfN[Array, Byte](n, byteGen)
      .map(ByteString.copyFrom)

  // TODO, Start this generator should Live on quivr
  val arbitraryDigest32: Arbitrary[Digest32] =
    Arbitrary(
      for {
        bs <- genSizedStrictByteString(32)()
      } yield Digest32(bs)
    )

  val arbitraryDigest64: Arbitrary[Digest64] =
    Arbitrary(
      for {
        bs <- genSizedStrictByteString(64)()
      } yield Digest64(bs)
    )
  // TODO, Finish this generator should Live on quivr

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
