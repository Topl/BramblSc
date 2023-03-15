package co.topl.crypto.signing

import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.utility.{Length, Sized}

/* Forked from https://github.com/input-output-hk/scrypto */

abstract private[signing] class EllipticCurveSignatureScheme[SK <: SigningKey, VK <: VerificationKey, SIG <: Signature, SeedLength <: Length](implicit seedLength: SeedLength) {

  val SignatureLength: Int
  val KeyLength: Int

  type SizedSeed = Sized.Strict[Bytes, SeedLength]

  def deriveKeyPairFromEntropy(entropy: Entropy, password: Option[String])(implicit
    entropyToSeed: EntropyToSeed[SeedLength] = EntropyToSeed.instances.pbkdf2Sha512[SeedLength]
  ): KeyPair = {
    val seed = entropyToSeed.toSeed(entropy, password)
    deriveKeyPairFromSeed(seed)
  }

  def deriveKeyPairFromSeed(seed: SizedSeed): KeyPair = {
    val secretKey = deriveSecretKeyFromSeed(seed)
    val verificationKey = getVerificationKey(secretKey)
    KeyPair(secretKey, verificationKey)
  }

  def deriveSecretKeyFromSeed(seed: SizedSeed): SK

  def sign(privateKey: SK, message: Message): SIG

  def verify(signature: SIG, message: Message, verifyKey: VK): Boolean

  def getVerificationKey(privateKey: SK): VK
}
