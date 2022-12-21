package co.topl.models

import org.scalacheck.Arbitrary
import scodec.bits.ByteVector

trait ModelGenerators {

  implicit val arbitraryBytes: Arbitrary[ByteVector] =
    Arbitrary(implicitly[Arbitrary[Array[Byte]]].arbitrary.map(ByteVector(_)))
}

object ModelGenerators extends ModelGenerators
