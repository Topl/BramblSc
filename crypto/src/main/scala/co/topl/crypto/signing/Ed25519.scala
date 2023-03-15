package co.topl.crypto.signing

import Ed25519.{PublicKey, SecretKey, seedLength}
import co.topl.crypto.utility.HasLength.instances._
import co.topl.crypto.utility.Sized
import co.topl.crypto.utility.Lengths._

/**
 * Implementation of Ed25519 elliptic curve signature
 */
class Ed25519 extends EllipticCurveSignatureScheme[SecretKey, PublicKey](seedLength) {
  private val impl = new eddsa.Ed25519
  impl.precompute()

  override val SignatureLength: Int = impl.SIGNATURE_SIZE
  override val KeyLength: Int = impl.SECRET_KEY_SIZE

  /**
   * Sign a given message with a given signing key.
   *
   * @param privateKey The private signing key
   * @param message a ByteVector that the the signature will be generated for
   * @return the signature
   */
  override def sign(privateKey: SecretKey, message: Message): Signature = {
    val sig = new Array[Byte](impl.SIGNATURE_SIZE)
    impl.sign(
      privateKey.bytes.data,
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
    val sigByteArray = signature
    val vkByteArray = publicKey.bytes.data
    val msgByteArray = message

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
  override def getVerificationKey(secretKey: SecretKey): PublicKey = {
    val pkBytes = new Array[Byte](impl.PUBLIC_KEY_SIZE)
    impl.generatePublicKey(secretKey.bytes.data, 0, pkBytes, 0)
    PublicKey(Sized.strictUnsafe(pkBytes))
  }

  /**
   * Derive an Ed25519 secret key from a seed.
   *
   * In accordance to RFC-8032 Section 5.1.5 any 32 byte value is a valid seed for Ed25519 signing.
   * Therefore we simply slice the first 32 bytes from the seed or pad the seed as necessary.
   *
   * @param seed the seed
   * @return the secret signing key
   **/
  override def deriveSecretKeyFromSeed(seed: Seed): SecretKey =
    SecretKey(Sized.strictUnsafe(seed.slice(0, 32).padTo(seedLength, 0.toByte)))
}
