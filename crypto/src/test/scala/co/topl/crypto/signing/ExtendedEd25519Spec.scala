package co.topl.crypto.signing

import co.topl.crypto.generation.{Bip32Index, Bip32Indexes}
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.generation.KeyInitializer.Instances.extendedEd25519Initializer
import co.topl.crypto.utils.EntropySupport._
import co.topl.crypto.utils.Hex.implicits.Ops
import co.topl.crypto.utils.{Hex, TestVector}
import io.circe.{Decoder, HCursor}
import io.circe.generic.semiauto.deriveDecoder
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.charset.StandardCharsets

class ExtendedEd25519Spec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519

  property("with ExtendedEd25519, signed message should be verifiable with appropriate public key") {
    forAll { (entropy1: Entropy, entropy: Entropy, message1: Array[Byte], message2: Array[Byte]) =>
      whenever(!(entropy1 == entropy) && !(message1 sameElements message2)) {
        val k1 = extendedEd25519.deriveKeyPairFromEntropy(entropy1, None)
        val k2 = extendedEd25519.deriveKeyPairFromEntropy(entropy, None)
        val sig = extendedEd25519.sign(k1.signingKey, message1)

        extendedEd25519.verify(sig, message1, k1.verificationKey) shouldBe true
        extendedEd25519.verify(sig, message1, k2.verificationKey) shouldBe false
        extendedEd25519.verify(sig, message2, k1.verificationKey) shouldBe false
      }
    }
  }

  property("with ExtendedEd25519, keyPairs generated with the same seed should be the same") {
    forAll { (entropy: Entropy) =>
      whenever(entropy.value.length != 0) {
        val keyPair1 = extendedEd25519.deriveKeyPairFromEntropy(entropy, None)
        val keyPair2 = extendedEd25519.deriveKeyPairFromEntropy(entropy, None)

        keyPair1 shouldBe keyPair2
      }
    }
  }

  property("Topl specific seed generation mechanism should generate a fixed secret key given an entropy and password") {
    val e = Entropy("topl".getBytes(StandardCharsets.UTF_8))
    val p = "topl"
    val specOutSK =
      ExtendedEd25519.SecretKey(
        "d8f0ad4d22ec1a143905af150e87c7f0dadd13749ef56fbd1bb380c37bc18c58".hexStringToBytes,
        "a900381746984a637dd3fa454419a6d560d14d4142921895575f406c9ad8d92d".hexStringToBytes,
        "cd07b700697afb30785ac4ab0ca690fd87223a12a927b4209ecf2da727ecd039".hexStringToBytes
      )
    val specOutVK =
      ExtendedEd25519.PublicKey(
        Ed25519.PublicKey("e684c4a4442a9e256b18460b74e0bdcd1c4c9a7f4c504e8555670f69290f142d".hexStringToBytes),
        "cd07b700697afb30785ac4ab0ca690fd87223a12a927b4209ecf2da727ecd039".hexStringToBytes
      )
    val specOut = KeyPair(specOutSK, specOutVK)

    val keys = extendedEd25519.deriveKeyPairFromEntropy(e, Some(p))

    keys shouldBe specOut
  }

  ExtendedEd25519SigningSpecHelper.testVectors.foreach { underTest =>
    property(s"${underTest.description}") {
      val vk = extendedEd25519.getVerificationKey(underTest.inputs.secretKey)
      val sig = extendedEd25519.sign(underTest.inputs.secretKey, underTest.inputs.message)

      extendedEd25519.verify(sig, underTest.inputs.message, vk) shouldBe true
      extendedEd25519.verify(sig, underTest.inputs.message, underTest.outputs.verificationKey) shouldBe true
      extendedEd25519.verify(underTest.outputs.signature, underTest.inputs.message, vk) shouldBe true
      extendedEd25519.verify(
        underTest.outputs.signature,
        underTest.inputs.message,
        underTest.outputs.verificationKey
      ) shouldBe true
    }
  }

  ExtendedEd25519CKDSpecHelper.testVectors.foreach { underTest =>
    property(s"${underTest.description}") {
      val derivedChild_keyPair =
        extendedEd25519.deriveKeyPairFromChildPath(underTest.inputs.rootSecretKey, underTest.inputs.path.toList)
      val derivedChild_xsk =
        underTest.inputs.path.foldLeft(underTest.inputs.rootSecretKey)((xsk, ind) =>
          extendedEd25519.deriveChildSecretKey(xsk, ind)
        )
      val fromDerivedChildSk_xvk = extendedEd25519.getVerificationKey(derivedChild_xsk)
      val derivedChild_xvk = underTest.inputs.rootVerificationKey.map { vk =>
        underTest.inputs.path.foldLeft(vk) {
          case (xvk, ind: Bip32Indexes.SoftIndex) =>
            extendedEd25519.deriveChildVerificationKey(xvk, ind)
          case _ => throw new Exception("received hardened index when soft index was expected")
        }
      }

      derivedChild_xsk shouldBe underTest.outputs.childSecretKey
      fromDerivedChildSk_xvk shouldBe underTest.outputs.childVerificationKey
      derivedChild_keyPair.signingKey shouldBe underTest.outputs.childSecretKey

      derivedChild_xvk.foreach { input_xvk =>
        input_xvk shouldBe underTest.outputs.childVerificationKey
        input_xvk shouldBe fromDerivedChildSk_xvk
      }
      derivedChild_keyPair.verificationKey shouldBe underTest.outputs.childVerificationKey
    }
  }

  object ExtendedEd25519SigningSpecHelper {
    case class SpecInputs(secretKey: ExtendedEd25519.SecretKey, message: Array[Byte])

    case class SpecOutputs(verificationKey: ExtendedEd25519.PublicKey, signature: Array[Byte])

    case class ExtendedEd25519SignTestVector(description: String, inputs: SpecInputs, outputs: SpecOutputs)
        extends TestVector

    implicit def inputsDecoder(implicit extendedEd25519: ExtendedEd25519): Decoder[SpecInputs] = (c: HCursor) =>
      for {
        sk <- c
          .get[String]("secretKey")
          .map(Hex.decode)
          .map(extendedEd25519Initializer.fromBytes)
        msg <- c.downField("message").as[String].map(Hex.decode)
      } yield SpecInputs(sk, msg)

    implicit def outputsDecoder: Decoder[SpecOutputs] = (c: HCursor) =>
      for {
        vk <- c
          .get[String]("verificationKey")
          .map { base16String =>
            val bytes = Hex.decode(base16String)
            ExtendedEd25519.PublicKey(
              Ed25519.PublicKey(bytes.slice(0, 32)),
              bytes.slice(32, 64)
            )
          }
        sig <- c
          .get[String]("signature")
          .map(Hex.decode)
      } yield SpecOutputs(vk, sig)

    implicit def testVectorDecoder: Decoder[ExtendedEd25519SignTestVector] =
      deriveDecoder[ExtendedEd25519SignTestVector]
    val testVectors: List[ExtendedEd25519SignTestVector] = TestVector.read("signing/ExtendedEd25519.json")
  }

  object ExtendedEd25519CKDSpecHelper {

    case class SpecInputs(
      rootSecretKey:       ExtendedEd25519.SecretKey,
      rootVerificationKey: Option[ExtendedEd25519.PublicKey],
      path:                Vector[Bip32Index]
    )

    case class SpecOutputs(
      childSecretKey:       ExtendedEd25519.SecretKey,
      childVerificationKey: ExtendedEd25519.PublicKey
    )

    case class ExtendedEd25519CKDTestVector(description: String, inputs: SpecInputs, outputs: SpecOutputs)
        extends TestVector

    implicit def inputsDecoder(implicit extendedEd25519: ExtendedEd25519): Decoder[SpecInputs] = (c: HCursor) =>
      for {
        rootSk <- c
          .get[String]("rootSecretKey")
          .map(Hex.decode)
          .map(extendedEd25519Initializer.fromBytes)
        rootVkString <- c.get[Option[String]]("rootVerificationKey")
        rootVkOpt = rootVkString.map { hexString =>
          val rootVkBytes = Hex.decode(hexString)
          ExtendedEd25519.PublicKey(
            Ed25519.PublicKey(rootVkBytes.slice(0, 32)),
            rootVkBytes.slice(32, 64)
          )
        }
        path <- c
          .get[Vector[(String, Long)]]("path")
          .map(_.map {
            case ("soft", index) => Bip32Indexes.SoftIndex(index)
            case ("hard", index) => Bip32Indexes.HardenedIndex(index)
            case _               => throw new Exception("how not to do?")
          })
      } yield SpecInputs(rootSk, rootVkOpt, path)

    implicit def outputsDecoder(implicit extendedEd25519: ExtendedEd25519): Decoder[SpecOutputs] = (c: HCursor) =>
      for {
        childSk <- c
          .get[String]("childSecretKey")
          .map(Hex.decode)
          .map(extendedEd25519Initializer.fromBytes)
        childVk <- c
          .get[String]("childVerificationKey")
          .map { base16String =>
            val bytes = Hex.decode(base16String)
            ExtendedEd25519.PublicKey(
              Ed25519.PublicKey(bytes.slice(0, 32)),
              bytes.slice(32, 64)
            )
          }
      } yield SpecOutputs(childSk, childVk)

    implicit def testVectorDecoder: Decoder[ExtendedEd25519CKDTestVector] =
      deriveDecoder[ExtendedEd25519CKDTestVector]

    val testVectors: List[ExtendedEd25519CKDTestVector] =
      TestVector.read("generation/Bip32-Ed25519_ChildKeyDerivation.json")
  }
}
