package co.topl.brambl.common

import cats.Order

import com.google.protobuf.ByteString

object OrderInstances {

  trait Instances {

    implicit val orderEntries: Order[(ByteString, BigInt)] = new Order[(ByteString, BigInt)] {

      implicit private val bytesOrdering: Ordering[ByteString] =
        Ordering.comparatorToOrdering(ByteString.unsignedLexicographicalComparator())

      def compare(x: (ByteString, BigInt), y: (ByteString, BigInt)): Int =
        bytesOrdering.compare(x._1, y._1) match {
          case 0 => x._2.compare(y._2)
          case i => i
        }
    }
  }
  object instances extends Instances

}
