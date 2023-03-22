package co.topl.crypto

package object signing {
  trait SigningKey
  trait VerificationKey
  case class KeyPair[SK <: SigningKey, VK <: VerificationKey](signingKey: SK, verificationKey: VK)
}
