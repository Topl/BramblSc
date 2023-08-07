package co.topl.crypto

import scala.language.implicitConversions

/* Forked from https://github.com/input-output-hk/scrypto */

package object accumulators {

  case class LeafData(value: Array[Byte])

  case class Side(value: Byte)

  /** Immutable empty array which can be used in many places to avoid allocations. */
  val EmptyByteArray = Array.empty[Byte]

}
