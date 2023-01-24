package co.topl.brambl.routines.signatures

import cats.implicits.catsSyntaxOptionId
import co.topl.crypto.signing.Ed25519
import com.google.protobuf.ByteString
import quivr.models.{KeyPair, SignableBytes, SigningKey, VerificationKey, Witness}
import scodec.bits.ByteVector

object Ed25519Signature extends Signing {
  override val routine: String = "ed25519"

  override def createKeyPair(seed: Array[Byte]): KeyPair = {
    val (sk, vk) = Ed25519.instance.deriveKeyPairFromSeed(ByteVector(seed))
    val skBytes = sk.toArray
    val vkBytes = vk.toArray
    KeyPair(
      VerificationKey(ByteString.copyFrom(vkBytes)).some,
      SigningKey(ByteString.copyFrom(skBytes)).some
    )
  }

  override def sign(sk: SigningKey, msg: SignableBytes): Witness = {
    val privateKey = ByteVector(sk.value.toByteArray)
    val message = ByteVector(msg.value.toByteArray)
    Witness(
      ByteString.copyFrom(Ed25519.instance.sign(privateKey, message).toArray)
    )
  }
}
