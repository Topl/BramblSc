package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

sealed trait SecretKey

//noinspection ScalaFileName
object SecretKeys {
  case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends SecretKey

  object Ed25519 {
    type Length = Lengths.bytes32.type
  }
}
