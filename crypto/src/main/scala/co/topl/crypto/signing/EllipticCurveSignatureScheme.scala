package co.topl.crypto.signing

import co.topl.crypto.generation.mnemonic.Entropy

/* Forked from https://github.com/input-output-hk/scrypto */

abstract private[signing] class EllipticCurveSignatureScheme[
  SK <: SigningKey,
  VK <: VerificationKey,
  SEED <: SizedSeed,
  SIG <: SizedSignature
]{

  def deriveKeyPairFromEntropy(entropy: Entropy, password: Option[String]): KeyPair =
    deriveKeyPairFromSeed(entropyToSeed(entropy, password))

  def deriveKeyPairFromSeed(seed: SEED): KeyPair = {
    val secretKey = deriveSecretKeyFromSeed(seed)
    val verificationKey = getVerificationKey(secretKey)
    KeyPair(secretKey, verificationKey)
  }

  def entropyToSeed(entropy: Entropy, password: Option[String]): SEED
  def deriveSecretKeyFromSeed(seed: SEED): SK

  def sign(privateKey: SK, message: Message): SIG

  def verify(signature: SIG, message: Message, verifyKey: VK): Boolean

  def getVerificationKey(privateKey: SK): VK
}
