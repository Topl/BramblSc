package co.topl.brambl.syntax

import co.topl.crypto.signing
import co.topl.crypto.signing.ExtendedEd25519
import com.google.protobuf.ByteString
import quivr.models.SigningKey.ExtendedEd25519Sk
import quivr.models.VerificationKey.{Ed25519Vk, ExtendedEd25519Vk}
import quivr.models.{KeyPair, SigningKey, VerificationKey}

import scala.language.implicitConversions

trait KeyPairSyntax {

  implicit def pbVkToCryptoVk(pbVk: VerificationKey.ExtendedEd25519Vk): ExtendedEd25519.PublicKey =
    ExtendedEd25519.PublicKey(
      signing.Ed25519.PublicKey(pbVk.vk.value.toByteArray),
      pbVk.chainCode.toByteArray
    )

  implicit def pbKeyPairToCryptoKeyPair(
    pbKeyPair: KeyPair
  ): signing.KeyPair[ExtendedEd25519.SecretKey, ExtendedEd25519.PublicKey] =
    signing.KeyPair(
      ExtendedEd25519.SecretKey(
        pbKeyPair.sk.sk.extendedEd25519.get.leftKey.toByteArray,
        pbKeyPair.sk.sk.extendedEd25519.get.rightKey.toByteArray,
        pbKeyPair.sk.sk.extendedEd25519.get.chainCode.toByteArray
      ),
      pbKeyPair.vk.vk.extendedEd25519.get
    )

  implicit def cryptoVkToPbVk(cryptoVk: ExtendedEd25519.PublicKey): VerificationKey.ExtendedEd25519Vk =
    ExtendedEd25519Vk(
      Ed25519Vk(ByteString.copyFrom(cryptoVk.vk.bytes)),
      ByteString.copyFrom(cryptoVk.chainCode)
    )

  implicit def cryptoToPbKeyPair(
    keyPair: signing.KeyPair[ExtendedEd25519.SecretKey, ExtendedEd25519.PublicKey]
  ): KeyPair = {
    val skCrypto = keyPair.signingKey
    val sk = ExtendedEd25519Sk(
      ByteString.copyFrom(skCrypto.leftKey),
      ByteString.copyFrom(skCrypto.rightKey),
      ByteString.copyFrom(skCrypto.chainCode)
    )
    KeyPair(
      VerificationKey(VerificationKey.Vk.ExtendedEd25519(keyPair.verificationKey)),
      SigningKey(SigningKey.Sk.ExtendedEd25519(sk))
    )
  }
}
