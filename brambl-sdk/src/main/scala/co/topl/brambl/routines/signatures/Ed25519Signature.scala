package co.topl.brambl.routines.signatures

import cats.implicits.catsSyntaxOptionId
import co.topl.crypto.signing.Ed25519
import com.google.protobuf.ByteString
import quivr.models.{KeyPair, SignableBytes, SigningKey, VerificationKey, Witness}
import quivr.models.SigningKey.Ed25519SigningKey
import quivr.models.VerificationKey.Ed25519VerificationKey

// TODO: The whole routines package may be removed in TSDK-437
object Ed25519Signature extends Signing {
  override val routine: String = "ed25519"

  override def createKeyPair(seed: Array[Byte]): KeyPair = {
    val instance = new Ed25519
    val keys = instance.deriveKeyPairFromSeed(seed.padTo(Ed25519.SeedLength, 0.toByte))
    val sk = SigningKey.Value.Ed25519(Ed25519SigningKey(ByteString.copyFrom(keys.signingKey.bytes)))
    val vk = VerificationKey.Value.Ed25519(Ed25519VerificationKey(ByteString.copyFrom(keys.verificationKey.bytes)))
    KeyPair(
      VerificationKey(vk),
      SigningKey(sk)
    )
  }

  override def sign(sk: SigningKey, msg: SignableBytes): Witness = {
    // TODO: Need to handle when sk.value.ed25519 is None...
    val privateKey = Ed25519.SecretKey(sk.value.ed25519.get.value.toByteArray)
    val message = msg.value.toByteArray
    Witness(
      ByteString.copyFrom((new Ed25519).sign(privateKey, message))
    )
  }
}
