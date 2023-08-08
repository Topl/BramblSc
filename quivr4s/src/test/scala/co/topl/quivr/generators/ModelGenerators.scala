package co.topl.quivr.generators

import com.google.protobuf.ByteString
import org.scalacheck.{Arbitrary, Gen}
import quivr.models.Digest

trait ModelGenerators {

  def genSizedStrictByteString(n: Int)(
    byteGen: Gen[Byte] = Gen.choose[Byte](0, 32)
  ): Gen[ByteString] =
    Gen
      .containerOfN[Array, Byte](n, byteGen)
      .map(ByteString.copyFrom)

  val arbitraryDigest: Arbitrary[Digest] =
    Arbitrary(
      for {
        bs <- genSizedStrictByteString(32)()
      } yield Digest(bs)
    )

}

object ModelGenerators extends ModelGenerators
