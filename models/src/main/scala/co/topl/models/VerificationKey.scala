package co.topl.models

import co.topl.models.utility.{Lengths, Sized}
import scodec.bits.ByteVector

sealed trait VerificationKey

object VerificationKeys {
  case class Ed25519(bytes: ByteVector) extends VerificationKey

  object Ed25519 {
    type Length = Lengths.bytes32.type
  }
}
