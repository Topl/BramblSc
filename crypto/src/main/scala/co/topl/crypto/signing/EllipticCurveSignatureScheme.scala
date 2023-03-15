package co.topl.crypto.signing

import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy

/* Forked from https://github.com/input-output-hk/scrypto */

abstract private[signing] class EllipticCurveSignatureScheme[SK <: SigningKey, VK <: VerificationKey](seedLength: Int) {

  val SignatureLength: Int
  val KeyLength: Int

  def deriveKeyPairFromEntropy(entropy: Entropy, password: Option[String])(implicit
    entropyToSeed: EntropyToSeed = EntropyToSeed.instances.pbkdf2Sha512(seedLength)
  ): KeyPair = {
    val seed = entropyToSeed.toSeed(entropy, password)
    deriveKeyPairFromSeed(seed)
  }

  def deriveSecretKeyFromSeed(seed: Seed): SK

  def deriveKeyPairFromSeed(seed: Seed): KeyPair = {
    val secretKey = deriveSecretKeyFromSeed(seed)
    val verificationKey = getVerificationKey(secretKey)
    KeyPair(secretKey, verificationKey)
  }

  def sign(privateKey: SK, message: Message): Signature

  def verify(signature: Signature, message: Message, verifyKey: VK): Boolean

  def getVerificationKey(privateKey: SK): VK
}
