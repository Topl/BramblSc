package co.topl.models

import scodec.bits.ByteVector

sealed trait VerificationKey

object VerificationKeys {
  case class Ed25519(bytes: ByteVector) extends VerificationKey
}
