package co.topl.crypto.encryption

import co.topl.crypto.encryption.cipher.Aes.Codecs._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import io.circe._
import io.circe.parser._
import io.circe.syntax.EncoderOps

class CodecSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("AES Params > Encode and Decode") {
    val expectParams: cipher.Aes.AesParams = ???
    val expectJsonStr: String =
      """
        |
        |""".stripMargin
    val expectJson: Json = parse(expectJsonStr).getOrElse(Json.Null)
    expectJson.isNull shouldBe false

    // Decode test
    val testParams = expectJson.as[cipher.Aes.AesParams]
    testParams.isRight shouldBe true
    testParams shouldBe Right(expectParams)
    // Encode test
    val testJson = expectParams.asJson
    testJson.isNull shouldBe false
    testJson shouldBe expectJson

    // Encode and Decode test
    val encodedFromDecoded = testParams.map(_.asJson).toOption
    encodedFromDecoded.isDefined shouldBe true
    encodedFromDecoded.get.isNull shouldBe false
    encodedFromDecoded.get shouldBe expectJson
    encodedFromDecoded.get.toString shouldBe expectJsonStr
  }
  property("AES Params > Decode fails with invalid JSON") {}
  property("SCrypt Params > Encode and Decode") {}
  property("SCrypt Params > Decode fails with invalid JSON") {}
  property("Cipher > AES > Encode and Decode") {}
  property("Cipher > AES > Decode fails with invalid label") {}

  property("Cipher > AES > Decode fails with invalid JSON") {
    // verify if underlying piece fails, the whole decode fails
  }
  property("KDF > SCrypt > Encode and Decode") {}
  property("KDF > SCrypt > Decode fails with invalid label") {}

  property("KDF > SCrypt > Decode fails with invalid JSON") {
    // verify if underlying piece fails, the whole decode fails
  }
  property("VaultStore > Encode and Decode") {}

  property("VaultStore > Decode fails with invalid JSON") {
    // verify if underlying piece fails, the whole decode fails
  }
  property("VaultStore.fromJson utility behaves like json.as[VaultStore]") {}
}
