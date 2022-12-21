package co.topl.crypto.signing

import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy
import scodec.bits.ByteVector

/* Forked from https://github.com/input-output-hk/scrypto */

abstract private[signing] class EllipticCurveSignatureScheme(seedLength: Int) {

  val SignatureLength: Int
  val KeyLength: Int

  def deriveKeyPairFromEntropy(entropy: Entropy, password: Option[String])(implicit
    entropyToSeed:                      EntropyToSeed = EntropyToSeed.instances.pbkdf2Sha512(seedLength)
  ): (ByteVector, ByteVector) = {
    val seed = entropyToSeed.toSeed(entropy, password)
    deriveKeyPairFromSeed(seed)
  }

  def deriveKeyPairFromSeed(seed: ByteVector): (ByteVector, ByteVector)

  def sign(privateKey: ByteVector, message: ByteVector): ByteVector

  def verify(signature: ByteVector, message: ByteVector, verifyKey: ByteVector): Boolean

  def getVerificationKey(privateKey: ByteVector): ByteVector
}
