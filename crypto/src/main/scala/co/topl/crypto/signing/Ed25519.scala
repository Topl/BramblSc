package co.topl.crypto.signing

import co.topl.models.{Proofs, SecretKeys, VerificationKeys}
import scodec.bits.ByteVector

import scala.annotation.unused

/**
 * Implementation of Ed25519 elliptic curve signature
 */
class Ed25519
    extends EllipticCurveSignatureScheme[
      SecretKeys.Ed25519,
      VerificationKeys.Ed25519,
      Proofs.Knowledge.Ed25519
    ](32) {
  private val impl = new eddsa.Ed25519
  impl.precompute()

  override val SignatureLength: Int = impl.SIGNATURE_SIZE
  override val KeyLength: Int = impl.SECRET_KEY_SIZE

  /**
   * Generate an Ed25519 key pair from the given seed.
   * @param seed a 32 byte long ByteVector
   * @return a tuple of a secret signing key and a public verification key
   */
  override def deriveKeyPairFromSeed(
    seed: ByteVector
  ): (SecretKeys.Ed25519, VerificationKeys.Ed25519) = {
    val secretKey = SecretKeys.Ed25519(seed)
    val verificationKey = getVerificationKey(secretKey)
    secretKey -> verificationKey
  }

  /**
   * Sign a given message with a given signing key.
   *
   * @param privateKey The private signing key
   * @param message a ByteVector that the the signature will be generated for
   * @return the signature
   */
  override def sign(privateKey: SecretKeys.Ed25519, message: ByteVector): Proofs.Knowledge.Ed25519 = {
    val sig = new Array[Byte](impl.SIGNATURE_SIZE)
    impl.sign(
      privateKey.bytes.toArray,
      0,
      message.toArray,
      0,
      message.toArray.length,
      sig,
      0
    )

    Proofs.Knowledge.Ed25519(ByteVector(sig))
  }

  /**
   * Verify a signature against a message using the public verification key.
   *
   * @param signature the signature to use for verification
   * @param message the message that the signature is expected to verify
   * @param publicKey The key to use for verification
   * @return true if the signature is verified; otherwise false.
   */
  override def verify(
    signature: Proofs.Knowledge.Ed25519,
    message:   ByteVector,
    publicKey: VerificationKeys.Ed25519
  ): Boolean = {
    val sigByteArray = signature.bytes.toArray
    val vkByteArray = publicKey.bytes.toArray
    val msgByteArray = message.toArray

    sigByteArray.length == impl.SIGNATURE_SIZE &&
    vkByteArray.length == impl.PUBLIC_KEY_SIZE &&
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
   * @param secretKey the secret key
   * @return the public verification key
   */
  override def getVerificationKey(secretKey: SecretKeys.Ed25519): VerificationKeys.Ed25519 = {
    val pkBytes = new Array[Byte](impl.PUBLIC_KEY_SIZE)
    impl.generatePublicKey(secretKey.bytes.toArray, 0, pkBytes, 0)
    VerificationKeys.Ed25519(ByteVector(pkBytes))
  }
}

object Ed25519 {

  /**
   * The singleton instance of Ed25519
   */
  @unused
  val instance = new Ed25519
}
