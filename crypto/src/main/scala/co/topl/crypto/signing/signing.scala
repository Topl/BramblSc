package co.topl.crypto

import co.topl.crypto.utility.{Lengths, Sized}

package object signing {
  type Bytes = Array[Byte]

  object Ed25519 {
    val seedLength = 32
    type Length = Lengths.`32`.type

    case class SecretKey(bytes: Sized.Strict[Bytes, Length]) extends SigningKey
    case class PublicKey(bytes: Sized.Strict[Bytes, Length]) extends VerificationKey

  }

  object ExtendedEd25519 {
    val seedLength = 96
    type LeftLength = Lengths.`32`.type
    type RightLength = Lengths.`32`.type
    type ChainCodeLength = Lengths.`32`.type

    case class SecretKey(
                                leftKey: Sized.Strict[Bytes, LeftLength],
                                rightKey: Sized.Strict[Bytes, RightLength],
                                chainCode: Sized.Strict[Bytes, ChainCodeLength]
                              ) extends SigningKey

    case class PublicKey(
                                vk: Ed25519.PublicKey,
                                chainCode: Sized.Strict[Bytes, ChainCodeLength]
                              ) extends VerificationKey
  }

  trait SigningKey
  trait VerificationKey
  case class KeyPair(signingKey: SigningKey, verificationKey: VerificationKey)

  type Message = Bytes
  type Signature = Bytes
  type Seed = Bytes
}
