package co.topl.brambl.routines.signatures

import cats.implicits.catsSyntaxOptionId
import co.topl.crypto.{PrivateKey, signatures}
import com.google.protobuf.ByteString
import quivr.models.{KeyPair, SignableBytes, SigningKey, VerificationKey, Witness}

object Curve25519Signature extends Signing {
  override val routine: String = "curve25519"

  override def createKeyPair(seed: Array[Byte]): KeyPair = {
    val (sk, vk) = signatures.Curve25519.createKeyPair(seed)
    val skBytes = sk.value
    val vkBytes = vk.value
    KeyPair(
      VerificationKey(ByteString.copyFrom(vkBytes)).some,
      SigningKey(ByteString.copyFrom(skBytes)).some
    )
  }

  override def sign(sk: SigningKey, msg: SignableBytes): Witness =
    Witness(ByteString.copyFrom(signatures.Curve25519.sign(PrivateKey(sk.value.toByteArray), msg.value.toByteArray).value))
}
