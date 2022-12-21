package co.topl

import io.estatico.newtype.macros.newsubtype
import scodec.bits.ByteVector

import scala.language.implicitConversions

package object models {
  type Eta = ByteVector
  type Evidence = ByteVector



  @newsubtype case class NetworkPrefix(value: Byte)

  type Digest32 = ByteVector
}
