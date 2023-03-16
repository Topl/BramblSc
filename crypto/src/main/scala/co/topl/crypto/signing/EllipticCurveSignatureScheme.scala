package co.topl.crypto.signing

import co.topl.crypto.generation.mnemonic.Entropy

/* Forked from https://github.com/input-output-hk/scrypto */

abstract private[signing] class EllipticCurveSignatureScheme[
  SK <: SigningKey,
  VK <: VerificationKey,
  SEED <: SizedSeed,
  SIG <: SizedSignature
] {

  /**
   * Generate a key pair from a given entropy and password.
   *
   * @param entropy    the entropy
   * @param passphrase the passphrase
   * @return the key pair
   */
  def deriveKeyPairFromEntropy(entropy: Entropy, passphrase: Option[String]): KeyPair =
    deriveKeyPairFromSeed(entropyToSeed(entropy, passphrase))

  /**
   * Derive a key pair from a seed.
   *
   * @param seed the seed
   * @return the key pair
   */
  def deriveKeyPairFromSeed(seed: SEED): KeyPair = {
    val secretKey = deriveSecretKeyFromSeed(seed)
    val verificationKey = getVerificationKey(secretKey)
    KeyPair(secretKey, verificationKey)
  }

  /**
   * Generate a seed from a given entropy and password.
   *
   * @param entropy    the entropy
   * @param passphrase the passphrase
   * @return the seed
   */
  def entropyToSeed(entropy: Entropy, passphrase: Option[String]): SEED

  /**
   * Derive a secret key from a seed.
   *
   * @param seed the seed
   * @return the secret signing key
   */
  def deriveSecretKeyFromSeed(seed: SEED): SK

  /**
   * Sign a given message with a given signing key.
   *
   * @param privateKey The private signing key
   * @param message    a message that the the signature will be generated for
   * @return the signature
   */
  def sign(privateKey: SK, message: Message): SIG

  /**
   * Verify a signature against a message using the public verification key.
   *
   * @param signature the signature to use for verification
   * @param message   the message that the signature is expected to verify
   * @param verifyKey The key to use for verification
   * @return true if the signature is verified; otherwise false.
   */
  def verify(signature: SIG, message: Message, verifyKey: VK): Boolean

  /**
   * Get the public key from the secret key
   *
   * @param privateKey the secret key
   * @return the public verification key
   */
  def getVerificationKey(privateKey: SK): VK
}
