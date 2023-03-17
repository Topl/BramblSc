package co.topl.crypto.signing

import co.topl.crypto.generation.{Bip32Index, Bip32Indexes}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import ExtendedEd25519.{PublicKey, SecretKey}

import java.nio.{ByteBuffer, ByteOrder}

/**
 * Implementation of ExtendedEd25519 elliptic curve signature
 */
class ExtendedEd25519 extends EllipticCurveSignatureScheme[SecretKey, PublicKey](ExtendedEd25519.SeedLength) {
  private val impl = ExtendedEd25519.Impl
  impl.precompute()

  /**
   * Sign a given message with a given signing key.
   *
   * @note Precondition: the private key must be a valid ExtednedEd25519 secret key
   * @note Postcondition: the signature must be a valid ExtendedEd25519 signature
   *
   * @param privateKey The private signing key
   * @param message    a message that the the signature will be generated for
   * @return the signature
   */
  override def sign(privateKey: SecretKey, message: Array[Byte]): Array[Byte] = {
    val resultSig = new Array[Byte](ExtendedEd25519.SignatureLength)
    val pk: Array[Byte] = new Array[Byte](ExtendedEd25519.PublicKeyLength)
    val ctx: Array[Byte] = Array.empty
    val phflag: Byte = 0x00
    val leftKeyDataArray = privateKey.leftKey
    val h: Array[Byte] = leftKeyDataArray ++ privateKey.rightKey
    val s: Array[Byte] = leftKeyDataArray
    val m: Array[Byte] = message

    impl.scalarMultBaseEncoded(privateKey.leftKey, pk, 0)
    impl.implSign(impl.sha512Digest, h, s, pk, 0, ctx, phflag, m, 0, m.length, resultSig, 0)

    resultSig
  }

  /**
   * Verify a signature against a message using the public verification key.
   *
   * @note Precondition: the public key must be a valid Ed25519 public key
   * @note Precondition: the signature must be a valid ExtendedEd25519 signature
   *
   * @param signature the signature to use for verification
   * @param message   the message that the signature is expected to verify
   * @param verifyKey The key to use for verification
   * @return true if the signature is verified; otherwise false.
   */
  def verify(signature: Array[Byte], message: Array[Byte], verifyKey: Ed25519.PublicKey): Boolean =
    verifyKey.bytes.length == ExtendedEd25519.PublicKeyLength &&
    signature.length == ExtendedEd25519.SignatureLength &&
    impl.verify(
      signature,
      0,
      verifyKey.bytes,
      0,
      message,
      0,
      message.length
    )

  /**
   * Verify a signature against a message using the public verification key.
   *
   * @note Precondition: the public key must be a valid ExtendedEd25519 public key
   * @note Precondition: the signature must be a valid ExtendedEd25519 signature
   *
   * @param signature the signature to use for verification
   * @param message   the message that the signature is expected to verify
   * @param verifyKey The key to use for verification
   * @return true if the signature is verified; otherwise false.
   */
  override def verify(
    signature: Array[Byte],
    message:   Array[Byte],
    verifyKey: PublicKey
  ): Boolean =
    signature.length == ExtendedEd25519.SignatureLength &&
    verifyKey.vk.bytes.length == ExtendedEd25519.PublicKeyLength &&
    impl.verify(
      signature,
      0,
      verifyKey.vk.bytes,
      0,
      message,
      0,
      message.length
    )

  /**
   * Deterministically derives a child secret key located at the given index.
   *
   * @note Precondition: the secret key must be a valid ExtendedEd25519 secret key
   * @note Postcondition: the secret key must be a valid ExtendedEd25519 secret key
   *
   * @param secretKey the secret key to derive the child key from
   * @param index the index of the key to derive
   * @return an extended secret key
   */
  def deriveChildSecretKey(
    secretKey: SecretKey,
    index:     Bip32Index
  ): SecretKey = {

    val lNum: BigInt = ExtendedEd25519.leftNumber(secretKey)
    val rNum: BigInt = ExtendedEd25519.rightNumber(secretKey)
    val public: PublicKey = getVerificationKey(secretKey)

    val zHmacData: Array[Byte] = index match {
      case _: Bip32Indexes.SoftIndex =>
        0x02.toByte +: (public.vk.bytes ++ index.bytes)
      case _: Bip32Indexes.HardenedIndex =>
        0x00.toByte +: (secretKey.leftKey ++ secretKey.rightKey ++ index.bytes)
    }
    val z = ExtendedEd25519.hmac512WithKey(secretKey.chainCode, zHmacData)

    val zLeft =
      BigInt(1, z.slice(0, 28).reverse)

    val zRight =
      BigInt(1, z.slice(32, 64).reverse)

    val nextLeft =
      ByteBuffer
        .wrap((zLeft * 8 + lNum).toByteArray.reverse)
        .order(ByteOrder.LITTLE_ENDIAN)
        .array()
        .take(32)

    val nextRight =
      ByteBuffer
        .wrap(((zRight + rNum) % (BigInt(2).pow(256))).toByteArray.reverse)
        .order(ByteOrder.LITTLE_ENDIAN)
        .array()
        .take(32)

    val chaincodeHmacData = index match {
      case _: Bip32Indexes.SoftIndex =>
        0x03.toByte +: (public.vk.bytes ++ index.bytes)
      case _: Bip32Indexes.HardenedIndex =>
        0x01.toByte +: (secretKey.leftKey ++ secretKey.rightKey ++ index.bytes)
    }

    val nextChainCode =
      ExtendedEd25519
        .hmac512WithKey(secretKey.chainCode, chaincodeHmacData)
        .slice(32, 64)

    SecretKey(nextLeft, nextRight, nextChainCode)
  }

  /**
   * Deterministically derives a child public key located at the given soft index.
   * Follows section V.D from the BIP32-ED25519 spec.
   *
   * @note Precondition: the verification key must be a valid ExtendedEd25519 public key
   * @note Postcondition: the verification key must be a valid ExtendedEd25519 public key
   *
   * @param verificationKey the public key to derive the child key from
   * @param index the index of the key to derive
   * @return an extended public key
   */
  def deriveChildVerificationKey(
    verificationKey: PublicKey,
    index:           Bip32Indexes.SoftIndex
  ): PublicKey = {

    val z = ExtendedEd25519.hmac512WithKey(
      verificationKey.chainCode,
      (((0x02: Byte) +: verificationKey.vk.bytes) ++ index.bytes)
    )

    val zL = z.slice(0, 28)

    val zLMult8 = ByteBuffer
      .wrap(
        (8 * BigInt(1, zL.reverse)).toByteArray.reverse
          .padTo(32, 0: Byte)
      )
      .order(ByteOrder.LITTLE_ENDIAN)
      .array()
      .take(32)

    val scaledZL = new impl.PointAccum
    impl.scalarMultBase(zLMult8, scaledZL)

    val publicKeyPoint = new impl.PointExt
    impl.decodePointVar(verificationKey.vk.bytes, 0, negate = false, publicKeyPoint)

    impl.pointAddVar(negate = false, publicKeyPoint, scaledZL)

    val nextPublicKeyBytes = new Array[Byte](ExtendedEd25519.PublicKeyLength)
    impl.encodePoint(scaledZL, nextPublicKeyBytes, 0)

    val nextChainCode =
      ExtendedEd25519
        .hmac512WithKey(
          verificationKey.chainCode,
          Array(0x03.toByte) ++ verificationKey.vk.bytes ++ index.bytes
        )
        .slice(32, 64)

    PublicKey(
      Ed25519.PublicKey(nextPublicKeyBytes),
      nextChainCode
    )
  }

  /**
   * Get the public key from the secret key
   *
   * @note Precondition: the secret key must be a valid ExtendedEd25519 secret key
   * @note Postcondition: the public key must be a valid ExtendedEd25519 public key
   *
   * @param secretKey the secret key
   * @return the public verification key
   */
  override def getVerificationKey(secretKey: SecretKey): PublicKey = {
    val pk = new Array[Byte](ExtendedEd25519.PublicKeyLength)
    impl.scalarMultBaseEncoded(secretKey.leftKey, pk, 0)

    PublicKey(
      Ed25519.PublicKey(pk),
      secretKey.chainCode
    )
  }

  /**
   * Derive an ExtendedEd25519 secret key from a seed.
   *
   * As defined in Section 3 of Khovratovich et. al. and detailed in CIP-0003, clamp bits to make a valid
   * Bip32-Ed25519 private key
   *
   * @note Precondition: the seed must have a length of 96 bytes
   *
   * @param seed the seed
   * @return the secret signing key
   */
  override def deriveSecretKeyFromSeed(seed: Array[Byte]): SecretKey = {
    require(
      seed.length == ExtendedEd25519.SeedLength,
      s"Invalid seed length. Expected: ${ExtendedEd25519.SeedLength}, Received: ${seed.length}"
    )
    ExtendedEd25519.clampBits(seed)
  }

}

object ExtendedEd25519 {
  private val Impl = new eddsa.Ed25519

  val SignatureLength: Int = Impl.SIGNATURE_SIZE
  val KeyLength: Int = Impl.SECRET_KEY_SIZE
  val PublicKeyLength: Int = Impl.PUBLIC_KEY_SIZE
  val SeedLength: Int = 96

  case class SecretKey(leftKey: Array[Byte], rightKey: Array[Byte], chainCode: Array[Byte]) extends SigningKey {

    require(
      leftKey.length == KeyLength,
      s"Invalid left key length. Expected: ${KeyLength}, Received: ${leftKey.length}"
    )

    require(
      rightKey.length == KeyLength,
      s"Invalid right key length. Expected: ${KeyLength}, Received: ${rightKey.length}"
    )

    require(
      chainCode.length == KeyLength,
      s"Invalid chain code length. Expected: ${KeyLength}, Received: ${chainCode.length}"
    )

    override def equals(that: Any): Boolean = that match {
      case that: SecretKey =>
        (leftKey sameElements that.leftKey) &&
        (rightKey sameElements that.rightKey) &&
        (chainCode sameElements that.chainCode)
      case _ => false
    }
  }

  case class PublicKey(vk: Ed25519.PublicKey, chainCode: Array[Byte]) extends VerificationKey {

    require(
      chainCode.length == KeyLength,
      s"Invalid chain code length. Expected: ${KeyLength}, Received: ${chainCode.length}"
    )

    override def equals(that: Any): Boolean = that match {
      case that: PublicKey =>
        (vk equals that.vk) && (chainCode sameElements that.chainCode)
      case _ => false
    }
  }

  /**
   * ED-25519 Base Order N
   *
   * Equivalent to `2^252 + 27742317777372353535851937790883648493`
   */
  private val edBaseN: BigInt = BigInt("7237005577332262213973186563042994240857116359379907606001950938285454250989")

  /**
   * Validates that the given key is a valid derived key.
   * Keys are invalid if their left private keys are divisible by the ED-25519 Base Order N.
   * @param value the private key value
   * @return either an invalid error or the private key
   */
  def validate(value: SecretKey): Either[InvalidDerivedKey, SecretKey] =
    Either.cond(leftNumber(value) % edBaseN != 0, value, InvalidDerivedKey)

  /**
   * clamp bits to make a valid Bip32-Ed25519 private key
   * As defined in Section 3 of Khovratovich et. al. and detailed in CIP-0003,
   * the following algorithm is applied to create valid ExtendedEd25519 secret keys .
   */
  private[ExtendedEd25519] def clampBits(
    sizedSeed: Array[Byte]
  ): SecretKey = {
    val seed = sizedSeed

    // turn seed into a valid ExtendedPrivateKeyEd25519 per the SLIP-0023 Icarus spec
    seed(0) = (seed(0) & 0xf8.toByte).toByte
    seed(31) = ((seed(31) & 0x1f.toByte) | 0x40.toByte).toByte

    SecretKey(
      seed.slice(0, 32),
      seed.slice(32, 64),
      seed.slice(64, 96)
    )
  }

  // Note: BigInt expects Big-Endian, but SLIP/BIP-ED25519 need Little-Endian
  private def leftNumber(secretKey: SecretKey): BigInt =
    BigInt(1, secretKey.leftKey.reverse)

  private def rightNumber(secretKey: SecretKey): BigInt =
    BigInt(1, secretKey.rightKey.reverse)

  private def hmac512WithKey(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
    val mac = new HMac(new SHA512Digest())
    mac.init(new KeyParameter(key))
    mac.update(data, 0, data.length)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    out
  }

  case object InvalidDerivedKey
  type InvalidDerivedKey = InvalidDerivedKey.type
}
