package co.topl.crypto.signing

import Ed25519.{PublicKey, SecretKey}

/**
 * Implementation of Ed25519 elliptic curve signature
 */
class Ed25519 extends EllipticCurveSignatureScheme[SecretKey, PublicKey](Ed25519.SeedLength) {
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
  override def sign(privateKey: SecretKey, message: Array[Byte]): Array[Byte] = {
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
    sig
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
    signature: Array[Byte],
    message:   Array[Byte],
    publicKey: PublicKey
  ): Boolean = {
    val sigByteArray = signature
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
   * Therefore, with the precondition on the seed size, we simply slice the first 32 bytes from the seed.
   *
   * @note Precondition: the seed must have a length of at least 32 bytes
   *
   * @param seed the seed
   * @return the secret signing key
   */
  override def deriveSecretKeyFromSeed(seed: Array[Byte]): SecretKey = {
    require(
      seed.length >= Ed25519.SeedLength,
      s"Invalid seed length. Expected: ${ExtendedEd25519.SeedLength}, Received: ${seed.length}"
    )
    SecretKey(seed.slice(0, 32))
  }
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

    override def equals(that: Any): Boolean = that match {
      case that: SecretKey => bytes sameElements that.bytes
      case _               => false
    }
  }

  case class PublicKey(bytes: Array[Byte]) extends VerificationKey {

    require(
      bytes.length == PublicKeyLength,
      s"Invalid right key length. Expected: ${PublicKeyLength}, Received: ${bytes.length}"
    )

    override def equals(that: Any): Boolean = that match {
      case that: PublicKey => bytes sameElements that.bytes
      case _               => false
    }
  }
}
