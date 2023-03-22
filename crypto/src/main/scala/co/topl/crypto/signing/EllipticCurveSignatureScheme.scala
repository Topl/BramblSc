package co.topl.crypto.signing

import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy

/* Forked from https://github.com/input-output-hk/scrypto */

abstract private[signing] class EllipticCurveSignatureScheme[
  SK <: SigningKey,
  VK <: VerificationKey
](seedLength: Int) {

  /**
   * Generate a key pair from a given entropy and password.
   *
   * @param entropy    the entropy
   * @param passphrase the passphrase
   * @return the key pair
   */
  def deriveKeyPairFromEntropy(entropy: Entropy, passphrase: Option[String])(implicit
    entropyToSeed: EntropyToSeed = EntropyToSeed.instances.pbkdf2Sha512(seedLength)
  ): KeyPair[SK, VK] = {
    val seed = entropyToSeed.toSeed(entropy, passphrase)
    deriveKeyPairFromSeed(seed)
  }

  /**
   * Derive a key pair from a seed.
   *
   * @param seed the seed
   * @return the key pair
   */
  def deriveKeyPairFromSeed(seed: Array[Byte]): KeyPair[SK, VK] = {
    val secretKey = deriveSecretKeyFromSeed(seed)
    val verificationKey = getVerificationKey(secretKey)
    KeyPair(secretKey, verificationKey)
  }

  /**
   * Derive a secret key from a seed.
   *
   * @param seed the seed
   * @return the secret signing key
   */
  def deriveSecretKeyFromSeed(seed: Array[Byte]): SK

  /**
   * Sign a given message with a given signing key.
   *
   * @param privateKey The private signing key
   * @param message    a message that the the signature will be generated for
   * @return the signature
   */
  def sign(privateKey: SK, message: Array[Byte]): Array[Byte]

  /**
   * Verify a signature against a message using the public verification key.
   *
   * @param signature the signature to use for verification
   * @param message   the message that the signature is expected to verify
   * @param verifyKey The key to use for verification
   * @return true if the signature is verified; otherwise false.
   */
  def verify(signature: Array[Byte], message: Array[Byte], verifyKey: VK): Boolean

  /**
   * Get the public key from the secret key
   *
   * @param privateKey the secret key
   * @return the public verification key
   */
  def getVerificationKey(privateKey: SK): VK
}
