package co.topl.brambl.common

import cats.Order
import co.topl.brambl.syntax.{
  GroupAndSeriesFungible,
  GroupFungible,
  GroupType,
  LvlType,
  SeriesFungible,
  SeriesType,
  ValueTypeIdentifier
}
import com.google.protobuf.ByteString

object OrderInstances {

  trait Instances {

    private val bytesOrdering: Ordering[ByteString] =
      Ordering.comparatorToOrdering(ByteString.unsignedLexicographicalComparator())

    implicit val orderValueTypeIdentifier: Order[ValueTypeIdentifier] = new Order[ValueTypeIdentifier] {

      override def compare(x: ValueTypeIdentifier, y: ValueTypeIdentifier): Int =
        (x, y) match {
          case (GroupType(a), GroupType(b))   => bytesOrdering.compare(a.value, b.value)
          case (SeriesType(a), SeriesType(b)) => bytesOrdering.compare(a.value, b.value)
          case (GroupAndSeriesFungible(a, b), GroupAndSeriesFungible(c, d)) =>
            bytesOrdering.compare(a.value.concat(b.value), c.value.concat(d.value))
          case (GroupFungible(a), GroupFungible(b))   => bytesOrdering.compare(a.value, b.value)
          case (SeriesFungible(a), SeriesFungible(b)) => bytesOrdering.compare(a.value, b.value)
          case (LvlType, _)                           => -1
          case (_, LvlType)                           => 1
          case (GroupType(_), _)                      => -1
          case (_, GroupType(_))                      => 1
          case (SeriesType(_), _)                     => -1
          case (_, SeriesType(_))                     => 1
          case (GroupAndSeriesFungible(_, _), _)      => -1
          case (_, GroupAndSeriesFungible(_, _))      => 1
          case (GroupFungible(_), _)                  => -1
          case (_, GroupFungible(_))                  => 1
          case (SeriesFungible(_), _)                 => -1
          case (_, SeriesFungible(_))                 => 1
          case _                                      => 0
        }
    }

    implicit val orderEntries: Order[(ValueTypeIdentifier, BigInt)] = new Order[(ValueTypeIdentifier, BigInt)] {

      def compare(x: (ValueTypeIdentifier, BigInt), y: (ValueTypeIdentifier, BigInt)): Int =
        orderValueTypeIdentifier.compare(x._1, y._1) match {
          case 0 => x._2.compare(y._2)
          case i => i
        }
    }
  }
  object instances extends Instances

}
