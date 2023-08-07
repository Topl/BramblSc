package co.topl.crypto.signing

import cats.implicits._
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.utils.EntropySupport._
import co.topl.crypto.utils.Hex.implicits._
import co.topl.crypto.utils.{Hex, TestVector}
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, HCursor}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.charset.StandardCharsets

/**
 * Reference - https://github.com/Topl/reference_crypto/tree/main/specs/crypto/signing/Ed25519
 *
 * Test vectors for Topl implementation of Ed25519 signature routine. Test vector values are same as those specified for Ed25519.
 * https://datatracker.ietf.org/doc/html/rfc8032#section-7.1
 *
 * All values below are Hex encoded byte representations unless otherwise specified.
 */
class Ed25519Spec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("with Ed25519, signed message should be verifiable with appropriate public key") {
    forAll { (seed1: Entropy, seed2: Entropy, message1: Array[Byte], message2: Array[Byte]) =>
      whenever((seed1 =!= seed2) && !(message1 sameElements message2)) {
        val ed25519 = new Ed25519
        val k1 = ed25519.deriveKeyPairFromEntropy(seed1, None)
        val k2 = ed25519.deriveKeyPairFromEntropy(seed2, None)
        val sig = ed25519.sign(k1.signingKey, message1)

        ed25519.verify(sig, message1, k1.verificationKey) shouldBe true
        ed25519.verify(sig, message1, k2.verificationKey) shouldBe false
        ed25519.verify(sig, message2, k1.verificationKey) shouldBe false
      }
    }
  }

  property("with Ed25519, keyPairs generated with the same seed should be the same") {
    forAll { (entropy: Entropy) =>
      whenever(entropy.value.length != 0) {
        val ed25519 = new Ed25519
        val keyPair1 = ed25519.deriveKeyPairFromEntropy(entropy, None)
        val keyPair2 = ed25519.deriveKeyPairFromEntropy(entropy, None)

        keyPair1 shouldBe keyPair2
      }
    }
  }

  property("Topl specific seed generation mechanism should generate a fixed secret key given an entropy and password") {
    val e = Entropy("topl".getBytes(StandardCharsets.UTF_8))
    val p = "topl"
    val specOutSK = "d8f0ad4d22ec1a143905af150e87c7f0dadd13749ef56fbd1bb380c37bc18cf8".hexStringToBytes
    val specOutVK = "8ecfec14ce183dd6e747724993a9ae30328058fd85fa1e3c6f996b61bb164fa8".hexStringToBytes
    val specOut = KeyPair(Ed25519.SecretKey(specOutSK), Ed25519.PublicKey(specOutVK))

    val underTest = new Ed25519
    val keys = underTest.deriveKeyPairFromEntropy(e, Some(p))

    keys shouldBe specOut
  }

  Ed25519SpecHelper.testVectors.foreach { underTest =>
    property(s"${underTest.description}") {
      val ed25519 = new Ed25519

      val vk = ed25519.getVerificationKey(underTest.inputs.secretKey)
      val sig = ed25519.sign(underTest.inputs.secretKey, underTest.inputs.message)

      ed25519.verify(sig, underTest.inputs.message, vk) shouldBe true
      ed25519.verify(sig, underTest.inputs.message, underTest.outputs.verificationKey) shouldBe true
      ed25519.verify(underTest.outputs.signature, underTest.inputs.message, vk) shouldBe true
      ed25519.verify(
        underTest.outputs.signature,
        underTest.inputs.message,
        underTest.outputs.verificationKey
      ) shouldBe true
    }
  }

  object Ed25519SpecHelper {
    case class SpecInputs(secretKey: Ed25519.SecretKey, message: Array[Byte])

    case class SpecOutputs(verificationKey: Ed25519.PublicKey, signature: Array[Byte])

    case class Ed25519TestVector(description: String, inputs: SpecInputs, outputs: SpecOutputs) extends TestVector

    implicit val inputsDecoder: Decoder[SpecInputs] = (c: HCursor) =>
      for {
        sk <- c
          .downField("secretKey")
          .as[String]
          .map(b => Hex.decode(b))
        msg <- c.downField("message").as[String].map(b => Hex.decode(b))
      } yield SpecInputs(Ed25519.SecretKey(sk), msg)

    implicit val outputsDecoder: Decoder[SpecOutputs] = (c: HCursor) =>
      for {
        vk <- c
          .downField("verificationKey")
          .as[String]
          .map(b => Hex.decode(b))
        sig <- c
          .downField("signature")
          .as[String]
          .map(b => Hex.decode(b))
      } yield SpecOutputs(Ed25519.PublicKey(vk), sig)

    implicit val testVectorDecoder: Decoder[Ed25519TestVector] = deriveDecoder[Ed25519TestVector]
    val testVectors: List[Ed25519TestVector] = TestVector.read("signing/Ed25519.json")
  }
}
