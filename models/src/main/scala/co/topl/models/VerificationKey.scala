package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

sealed trait VerificationKey

object VerificationKeys {
  case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends VerificationKey

  object Ed25519 {
    type Length = Lengths.bytes32.type
  }
}
