package co.topl.brambl.routines.signatures

import cats.implicits.catsSyntaxOptionId
import co.topl.crypto.signing.Ed25519
import com.google.protobuf.ByteString
import quivr.models.{KeyPair, SignableBytes, SigningKey, VerificationKey, Witness}

object Ed25519Signature extends Signing {
  override val routine: String = "ed25519"

  override def createKeyPair(seed: Array[Byte]): KeyPair = {
    val instance = new Ed25519
    val keys = instance.deriveKeyPairFromSeed(seed.padTo(Ed25519.SeedLength, 0.toByte))
    val skBytes = keys.signingKey.bytes
    val vkBytes = keys.verificationKey.bytes
    KeyPair(
      VerificationKey(ByteString.copyFrom(vkBytes)),
      SigningKey(ByteString.copyFrom(skBytes))
    )
  }

  override def sign(sk: SigningKey, msg: SignableBytes): Witness = {
    val privateKey = Ed25519.SecretKey(sk.value.toByteArray)
    val message = msg.value.toByteArray
    Witness(
      ByteString.copyFrom((new Ed25519).sign(privateKey, message))
    )
  }
}
