package co.topl.crypto.signing

import Ed25519.{PublicKey, SecretKey, Seed, Signature}
import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy

/**
 * Implementation of Ed25519 elliptic curve signature
 */
class Ed25519 extends EllipticCurveSignatureScheme[SecretKey, PublicKey, Seed, Signature] {
  private val impl = Ed25519.Impl
  impl.precompute()

  /**
   * Sign a given message with a given signing key.
   *
   * @note Precondition: the private key must be a valid Ed25519 secret key - thus having a length of 32 bytes
   * @note Postcondition: the signature must be a valid Ed25519 signature - thus having a length of 64 bytes
   *
   * @param privateKey The private signing key
   * @param message a message that the the signature will be generated for
   * @return the signature
   */
  override def sign(privateKey: SecretKey, message: Message): Signature = {
    val sig = new Array[Byte](Ed25519.SignatureLength)
    impl.sign(
      privateKey.bytes,
      0,
      message,
      0,
      message.length,
      sig,
      0
    )
    Signature(sig)
  }

  /**
   * Verify a signature against a message using the public verification key.
   *
   * @note Precondition: the public key must be a valid Ed25519 public key
   * @note Precondition: the signature must be a valid Ed25519 signature
   *
   * @param signature the signature to use for verification
   * @param message the message that the signature is expected to verify
   * @param publicKey The key to use for verification
   * @return true if the signature is verified; otherwise false.
   */
  override def verify(
    signature: Signature,
    message:   Message,
    publicKey: PublicKey
  ): Boolean = {
    val sigByteArray = signature.bytes
    val vkByteArray = publicKey.bytes
    val msgByteArray = message

    sigByteArray.length == Ed25519.SignatureLength &&
    vkByteArray.length == Ed25519.PublicKeyLength &&
    impl.verify(
      sigByteArray,
      0,
      vkByteArray,
      0,
      msgByteArray,
      0,
      msgByteArray.length
    )
  }

  /**
   * Get the public key from the secret key
   *
   * @note Precondition: the secret key must be a valid Ed25519 secret key - thus having a length of 32 bytes
   * @note Postcondition: the public key must be a valid Ed25519 public key - thus having a length of 32 bytes
   *
   * @param secretKey the secret key
   * @return the public verification key
   */
  override def getVerificationKey(secretKey: SecretKey): PublicKey = {
    val pkBytes = new Array[Byte](Ed25519.PublicKeyLength)
    impl.generatePublicKey(secretKey.bytes, 0, pkBytes, 0)
    PublicKey(pkBytes)
  }

  /**
   * Derive an Ed25519 secret key from a seed.
   *
   * In accordance to RFC-8032 Section 5.1.5 any 32 byte value is a valid seed for Ed25519 signing.
   * Therefore, with the precondition on the seed size, the seed is a valid secret key.
   *
   * @note Precondition: the seed must have a length of 32 bytes
   *
   * @param seed the seed
   * @return the secret signing key
   */
  override def deriveSecretKeyFromSeed(seed: Seed): SecretKey = SecretKey(seed.bytes)

  /**
   * Generate a seed from a given entropy and password.
   *
   * @note Postcondition: the seed must have a length of 32 bytes
   *
   * @param entropy the entropy
   * @param passphrase the passphrase
   * @return the seed
   */
  override def entropyToSeed(entropy: Entropy, passphrase: Option[String]): Seed = Seed(
    EntropyToSeed.instances
      .pbkdf2Sha512(ExtendedEd25519.SeedLength)
      .toSeed(entropy, passphrase)
  )
}

object Ed25519 {
  private val Impl = new eddsa.Ed25519

  val SignatureLength: Int = Impl.SIGNATURE_SIZE
  val KeyLength: Int = Impl.SECRET_KEY_SIZE
  val PublicKeyLength: Int = Impl.PUBLIC_KEY_SIZE
  val SeedLength: Int = 32

  case class SecretKey(bytes: Array[Byte]) extends SigningKey {

    require(
      bytes.length == KeyLength,
      s"Invalid left key length. Expected: ${KeyLength}, Received: ${bytes.length}"
    )
  }

  case class PublicKey(bytes: Array[Byte]) extends VerificationKey {

    require(
      bytes.length == PublicKeyLength,
      s"Invalid right key length. Expected: ${PublicKeyLength}, Received: ${bytes.length}"
    )
  }

  case class Seed(override val bytes: Bytes) extends SizedSeed(SeedLength)

  case class Signature(override val bytes: Bytes) extends SizedSignature(SignatureLength)

}
