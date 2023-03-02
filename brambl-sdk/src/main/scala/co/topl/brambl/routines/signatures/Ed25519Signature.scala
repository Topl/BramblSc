package co.topl.brambl.routines.signatures

import cats.implicits.catsSyntaxOptionId
import co.topl.crypto.signing.Ed25519
import com.google.protobuf.ByteString
import quivr.models.{KeyPair, SignableBytes, SigningKey, VerificationKey, Witness}
import scodec.bits.ByteVector

object Ed25519Signature extends Signing {
  override val routine: String = "ed25519"

  override def createKeyPair(seed: Array[Byte]): KeyPair = {
    val instance = new Ed25519
    val (sk, vk) = instance.deriveKeyPairFromSeed(ByteVector(seed.padTo(instance.seedLength, 0.toByte)))
    val skBytes = sk.toArray
    val vkBytes = vk.toArray
    KeyPair(
      VerificationKey(ByteString.copyFrom(vkBytes)),
      SigningKey(ByteString.copyFrom(skBytes))
    )
  }

  override def sign(sk: SigningKey, msg: SignableBytes): Witness = {
    val privateKey = ByteVector(sk.value.toByteArray)
    val message = ByteVector(msg.value.toByteArray)
    Witness(
      ByteString.copyFrom((new Ed25519).sign(privateKey, message).toArray)
    )
  }
}
