package co.topl.crypto

import co.topl.crypto.utility.{Lengths, Sized}

package object signing {
  type Bytes = Array[Byte]

  trait SigningKey
  trait VerificationKey
  trait Signature
  case class KeyPair(signingKey: SigningKey, verificationKey: VerificationKey)

  type Message = Bytes
  type Seed = Bytes
}
