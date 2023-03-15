package co.topl.crypto.signing

import co.topl.crypto.generation.{Bip32Index, Bip32Indexes, EntropyToSeed, Pbkdf2Sha512}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import ExtendedEd25519.{PublicKey, SecretKey, SizedSignature, SeedLength}
import co.topl.crypto.utility.HasLength.instances._
import co.topl.crypto.utility.{Length, Lengths, Sized}
import co.topl.crypto.utility.Lengths._

import java.nio.{ByteBuffer, ByteOrder}

class ExtendedEd25519
  extends EllipticCurveSignatureScheme[SecretKey, PublicKey, SizedSignature, SeedLength] {

  private val impl = new eddsa.Ed25519

  override val SignatureLength: Int = impl.SIGNATURE_SIZE
  override val KeyLength: Int = impl.SECRET_KEY_SIZE
  val PublicKeyLength: Int = impl.PUBLIC_KEY_SIZE

  override def sign(privateKey: SecretKey, message: Message): SizedSignature = {
    val resultSig = new Array[Byte](SignatureLength)
    val pk: Array[Byte] = new Array[Byte](PublicKeyLength)
    val ctx: Array[Byte] = Array.empty
    val phflag: Byte = 0x00
    val leftKeyDataArray = privateKey.leftKey.data
    val h: Array[Byte] = leftKeyDataArray ++ privateKey.rightKey.data
    val s: Array[Byte] = leftKeyDataArray
    val m: Array[Byte] = message

    impl.scalarMultBaseEncoded(privateKey.leftKey.data, pk, 0)
    impl.implSign(impl.sha512Digest, h, s, pk, 0, ctx, phflag, m, 0, m.length, resultSig, 0)

    SizedSignature(Sized.strictUnsafe(resultSig))
  }

  def verify(signature: SizedSignature, message: Message, verifyKey: Ed25519.PublicKey): Boolean =
    verifyKey.bytes.data.length == PublicKeyLength &&
      signature.bytes.data.length == SignatureLength &&
      impl.verify(
        signature.bytes.data,
        0,
        verifyKey.bytes.data,
        0,
        message,
        0,
        message.length
      )

  override def verify(
                       signature: SizedSignature,
                       message:   Message,
                       verifyKey: PublicKey
                     ): Boolean =
    signature.bytes.data.length == SignatureLength &&
      verifyKey.vk.bytes.data.length == PublicKeyLength &&
      impl.verify(
        signature.bytes.data,
        0,
        verifyKey.vk.bytes.data,
        0,
        message,
        0,
        message.length
      )

  def deriveSecret(
                    secretKey: SecretKey,
                    index:     Bip32Index
                  ): SecretKey = {

    val lNum: BigInt = ExtendedEd25519.leftNumber(secretKey)
    val rNum: BigInt = ExtendedEd25519.rightNumber(secretKey)
    val public: PublicKey = getVerificationKey(secretKey)

    val zHmacData: Bytes = index match {
      case _: Bip32Indexes.SoftIndex =>
        0x02.toByte +: (public.vk.bytes.data ++ index.bytes)
      case _: Bip32Indexes.HardenedIndex =>
        0x00.toByte +: (secretKey.leftKey.data ++ secretKey.rightKey.data ++ index.bytes)
    }
    val z = ExtendedEd25519.hmac512WithKey(secretKey.chainCode.data, zHmacData)

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
        0x03.toByte +: (public.vk.bytes.data ++ index.bytes)
      case _: Bip32Indexes.HardenedIndex =>
        0x01.toByte +: (secretKey.leftKey.data ++ secretKey.rightKey.data ++ index.bytes)
    }

    val nextChainCode =
        ExtendedEd25519
          .hmac512WithKey(secretKey.chainCode.data, chaincodeHmacData)
          .slice(32, 64)

    SecretKey(
      Sized.strictUnsafe(nextLeft),
      Sized.strictUnsafe(nextRight),
      Sized.strictUnsafe(nextChainCode)
    )
  }

  /**
   * Deterministically derives a child public key located at the given soft index.
   *
   * Follows section V.D from the BIP32-ED25519 spec.
   *
   * @param index the index of the key to derive
   * @return an extended public key
   */
  def deriveVerification(
                          verificationKey: PublicKey,
                          index:           Bip32Indexes.SoftIndex
                        ): PublicKey = {

    val z = ExtendedEd25519.hmac512WithKey(
      verificationKey.chainCode.data,
      (((0x02: Byte) +: verificationKey.vk.bytes.data) ++ index.bytes)
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
    impl.decodePointVar(verificationKey.vk.bytes.data, 0, negate = false, publicKeyPoint)

    impl.pointAddVar(negate = false, publicKeyPoint, scaledZL)

    val nextPublicKeyBytes = new Array[Byte](PublicKeyLength)
    impl.encodePoint(scaledZL, nextPublicKeyBytes, 0)

    val nextChainCode =
      ExtendedEd25519
        .hmac512WithKey(
          verificationKey.chainCode.data,
          Array(0x03.toByte) ++ verificationKey.vk.bytes.data ++ index.bytes
        )
        .slice(32, 64)

    PublicKey(
      Ed25519.PublicKey(Sized.strictUnsafe(nextPublicKeyBytes)),
      Sized.strictUnsafe(nextChainCode)
    )
  }

  override def getVerificationKey(secretKey: SecretKey): PublicKey = {
    val pk = new Array[Byte](PublicKeyLength)
    impl.scalarMultBaseEncoded(secretKey.leftKey.data, pk, 0)

    PublicKey(
      Ed25519.PublicKey(Sized.strictUnsafe(pk)),
      secretKey.chainCode
    )
  }

  def precompute(): Unit = impl.precompute()

  /**
   * Derive an ExtendedEd25519 secret key from a seed.
   *
   * As defined in Section 3 of Khovratovich et. al. and detailed in CIP-0003,
   * the following algorithm is applied to create valid ExtendedEd25519 secret keys [reference].
   *
   * @param seed the seed
   * @return the secret signing key
   */
  override def deriveSecretKeyFromSeed(seed: SizedSeed): SecretKey = ExtendedEd25519.clampBits(seed)
}

object ExtendedEd25519 {
  type LeftLength = Lengths.`32`.type
  type RightLength = Lengths.`32`.type
  type ChainCodeLength = Lengths.`32`.type
  type SignatureLength = Lengths.`64`.type
  type SeedLength = Lengths.`96`.type

  case class SecretKey(
                        leftKey: Sized.Strict[Bytes, LeftLength],
                        rightKey: Sized.Strict[Bytes, RightLength],
                        chainCode: Sized.Strict[Bytes, ChainCodeLength]
                      ) extends SigningKey

  case class PublicKey(
                        vk: Ed25519.PublicKey,
                        chainCode: Sized.Strict[Bytes, ChainCodeLength]
                      ) extends VerificationKey

  case class SizedSignature(bytes: Sized.Strict[Bytes, SignatureLength]) extends Signature

  def precomputed(): ExtendedEd25519 = {
    val instance = new ExtendedEd25519
    instance.precompute()
    instance
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

  /** clamp bits to make a valid Bip32-Ed25519 private key */
  private[ExtendedEd25519] def clampBits(
                                          sizedSeed: Sized.Strict[Bytes, SeedLength]
                                        ): SecretKey = {
    val seed = sizedSeed.data

    // turn seed into a valid ExtendedPrivateKeyEd25519 per the SLIP-0023 Icarus spec
    seed(0) = (seed(0) & 0xf8.toByte).toByte
    seed(31) = ((seed(31) & 0x1f.toByte) | 0x40.toByte).toByte

    SecretKey(
      Sized.strictUnsafe(seed.slice(0, 32)),
      Sized.strictUnsafe(seed.slice(32, 64)),
      Sized.strictUnsafe(seed.slice(64, 96))
    )
  }

  // Note: BigInt expects Big-Endian, but SLIP/BIP-ED25519 need Little-Endian
  private def leftNumber(secretKey: SecretKey): BigInt =
    BigInt(1, secretKey.leftKey.data.reverse)

  private def rightNumber(secretKey: SecretKey): BigInt =
    BigInt(1, secretKey.rightKey.data.reverse)

  private def hmac512WithKey(key: Array[Byte], data: Array[Byte]): Bytes = {
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