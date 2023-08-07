package co.topl.crypto.generation

import cats.scalatest.EitherValues
import co.topl.crypto.generation.mnemonic._
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
import co.topl.crypto.utils.TestVector
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, HCursor}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.crypto.utils.Hex.implicits.Ops

/**
 * test vectors adapted from multiple sources:
 * https://github.com/cardano-foundation/CIPs/blob/master/CIP-0003/Icarus.md#test-vectors
 * https://github.com/input-output-hk/rust-cardano/blob/9fad3d12341acc2ab0f9c2026149af3d839447e4/cardano/src/bip/test_vectors/bip39_english.txt
 */

class KeyInitializerSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with EitherValues {
  import KeyInitializer.Instances._
  implicit val ed25519Instance: Ed25519 = new Ed25519
  implicit val extendedEd25519Instance: ExtendedEd25519 = new ExtendedEd25519
  case class SpecInputs(mnemonic: String, size: MnemonicSize, password: Option[String])

  case class SpecOutputs(
    ed25519:         Ed25519.SecretKey,
    extendedEd25519: ExtendedEd25519.SecretKey
  )
  case class KeyInitializorTestVector(inputs: SpecInputs, outputs: SpecOutputs) extends TestVector

  implicit val inputsDecoder: Decoder[SpecInputs] = (c: HCursor) =>
    EntropyTestVectorHelper
      .mnemonicStringAndSize(c)
      .flatMap { case (mnemonicString, size) =>
        { c.downField("password").as[Option[String]] }
          .map { password => SpecInputs(mnemonicString, size, password) }
      }

  implicit val outputsDecoder: Decoder[SpecOutputs] = (c: HCursor) =>
    for {
      ed25519 <- c
        .get[String]("ed25519")
        .map(_.hexStringToBytes)
        .map(ed25519Initializer.fromBytes)
      extendedEd25519 <- c
        .get[String]("extendedEd25519")
        .map(_.hexStringToBytes)
        .map(extendedEd25519Initializer.fromBytes)
    } yield SpecOutputs(ed25519, extendedEd25519)

  implicit val testVectorDecoder: Decoder[KeyInitializorTestVector] = deriveDecoder[KeyInitializorTestVector]

  val testVectors: List[KeyInitializorTestVector] = TestVector.read("generation/KeyInitializer.json")

  testVectors.foreach { underTest =>
    property(
      s"Generate 96 byte seed from mnemonic: ${underTest.inputs.mnemonic} + password: ${underTest.inputs.password}"
    ) {
      val actualEd25519Sk = ed25519Initializer
        .fromMnemonicString(underTest.inputs.mnemonic)(Language.English, underTest.inputs.password)
        .value

      val actualExtended25519Sk = extendedEd25519Initializer
        .fromMnemonicString(underTest.inputs.mnemonic)(Language.English, underTest.inputs.password)
        .value

      actualEd25519Sk shouldBe underTest.outputs.ed25519
      actualExtended25519Sk shouldBe underTest.outputs.extendedEd25519
    }
  }
}
