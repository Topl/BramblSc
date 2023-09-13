package co.topl.brambl.common

import cats.Monoid
import com.google.protobuf.ByteString

object MonoidInstances {

  trait Instances {

    implicit val byteStringMonoid: Monoid[ByteString] = new Monoid[ByteString] {
      def empty: ByteString = ByteString.EMPTY

      def combine(x: ByteString, y: ByteString): ByteString = x.concat(y)
    }

  }
  object instances extends Instances
}
