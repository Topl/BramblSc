package co.topl.crypto.signing

import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.models.{Proof, SecretKey, VerificationKey}
import scodec.bits.ByteVector

/* Forked from https://github.com/input-output-hk/scrypto */

abstract private[signing] class EllipticCurveSignatureScheme[
  SK <: SecretKey,
  VK <: VerificationKey,
  SIG <: Proof
](seedLength: Int) {

  val SignatureLength: Int
  val KeyLength: Int

  def deriveKeyPairFromEntropy(entropy: Entropy, password: Option[String])(implicit
    entropyToSeed:                      EntropyToSeed = EntropyToSeed.instances.pbkdf2Sha512(seedLength)
  ): (SK, VK) = {
    val seed = entropyToSeed.toSeed(entropy, password)
    deriveKeyPairFromSeed(seed)
  }

  def deriveKeyPairFromSeed(seed: ByteVector): (SK, VK)

  def sign(privateKey: SK, message: ByteVector): SIG

  def verify(signature: SIG, message: ByteVector, verifyKey: VK): Boolean

  def getVerificationKey(privateKey: SK): VK
}
