package co.topl.crypto.encryption

import cats.Id
import co.topl.crypto.encryption.cipher.Aes.Codecs._
import co.topl.crypto.encryption.cipher.Codecs._
import co.topl.crypto.encryption.cipher.Aes
import co.topl.crypto.encryption.kdf.SCrypt.Codecs._
import co.topl.crypto.encryption.kdf.Codecs._
import co.topl.crypto.encryption.kdf.SCrypt
import co.topl.crypto.encryption.VaultStore.Codecs._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import io.circe._
import io.circe.syntax.EncoderOps

class CodecSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("AES Params > Encode and Decode") {
    // Decode test
    val testParams = Helpers.ExpectedAesParams.json.as[Aes.AesParams]
    testParams.isRight shouldBe true
    testParams shouldBe (Right(Helpers.ExpectedAesParams.value))
    // Encode test
    val testJson = Helpers.ExpectedAesParams.value.asJson
    testJson.isNull shouldBe false
    testJson shouldBe Helpers.ExpectedAesParams.json

    // Decode then Encode test
    val encodedFromDecoded = testParams.map(_.asJson).toOption
    encodedFromDecoded.isDefined shouldBe true
    encodedFromDecoded.get.isNull shouldBe false
    encodedFromDecoded.get shouldBe Helpers.ExpectedAesParams.json

    // Encode then Decode test
    val decodedFromEncoded = testJson.as[Aes.AesParams]
    decodedFromEncoded.isRight shouldBe true
    decodedFromEncoded shouldBe Right(Helpers.ExpectedAesParams.value)
  }

  property("AES Params > Decode fails with invalid JSON") {
    val invalidJson = Json.obj(
      "iv" -> Json.fromInt(10) // IV should be a string
    )
    val testParams = invalidJson.as[Aes.AesParams]
    testParams.isLeft shouldBe true
    testParams.swap.toOption.get shouldBe a[DecodingFailure]
  }

  property("SCrypt Params > Encode and Decode") {
    // Decode test
    val testParams = Helpers.ExpectedSCryptParams.json.as[SCrypt.SCryptParams]
    testParams.isRight shouldBe true
    testParams shouldBe Right(Helpers.ExpectedSCryptParams.value)
    // Encode test
    val testJson = Helpers.ExpectedSCryptParams.value.asJson
    testJson.isNull shouldBe false
    testJson shouldBe Helpers.ExpectedSCryptParams.json

    // Decode then Encode test
    val encodedFromDecoded = testParams.map(_.asJson).toOption
    encodedFromDecoded.isDefined shouldBe true
    encodedFromDecoded.get.isNull shouldBe false
    encodedFromDecoded.get shouldBe Helpers.ExpectedSCryptParams.json

    // Encode then Decode test
    val decodedFromEncoded = testJson.as[SCrypt.SCryptParams]
    decodedFromEncoded.isRight shouldBe true
    decodedFromEncoded shouldBe Right(Helpers.ExpectedSCryptParams.value)
  }

  property("SCrypt Params > Decode fails with invalid JSON") {
    // required field "n" is missing
    val invalidJson = Json.obj(
      "salt"  -> Json.fromString("salt"),
      "r"     -> Json.fromInt(10),
      "p"     -> Json.fromInt(10),
      "dkLen" -> Json.fromInt(10)
    )
    val testParams = invalidJson.as[SCrypt.SCryptParams]
    testParams.isLeft shouldBe true
    testParams.swap.toOption.get shouldBe a[DecodingFailure]
  }

  property("Cipher > AES > Encode and Decode") {
    // Decode test
    val testCipher = Helpers.ExpectedCipher.json.as[cipher.Cipher[Id]]
    testCipher.isRight shouldBe true
    testCipher shouldBe Right(Helpers.ExpectedCipher.value)
    // Encode test
    val testJson = Helpers.ExpectedCipher.value.asJson
    testJson.isNull shouldBe false
    testJson shouldBe Helpers.ExpectedCipher.json

    // Decode then Encode test
    val encodedFromDecoded = testCipher.map(_.asJson).toOption
    encodedFromDecoded.isDefined shouldBe true
    encodedFromDecoded.get.isNull shouldBe false
    encodedFromDecoded.get shouldBe Helpers.ExpectedCipher.json

    // Encode then Decode test
    val decodedFromEncoded = testJson.as[cipher.Cipher[Id]]
    decodedFromEncoded.isRight shouldBe true
    decodedFromEncoded shouldBe Right(Helpers.ExpectedCipher.value)
  }

  property("Cipher > AES > Decode fails with invalid label") {
    val fields = Helpers.ExpectedAesParams.fields :+ ("cipher" -> Json.fromString("invalid-label"))
    val invalidJson = Json.fromFields(fields)

    val testCipher = invalidJson.as[cipher.Cipher[Id]]
    testCipher.isLeft shouldBe true
    testCipher.swap.toOption.get shouldBe a[DecodingFailure]
  }

  property("Cipher > AES > Decode fails with invalid JSON") {
    // verify if underlying piece fails, the whole decode fails
    val fields = List("cipher" -> Json.fromString(Helpers.ExpectedAesParams.value.cipher)) // IV is missing
    val invalidJson = Json.fromFields(fields)

    val testCipher = invalidJson.as[cipher.Cipher[Id]]
    testCipher.isLeft shouldBe true
    testCipher.swap.toOption.get shouldBe a[DecodingFailure]
  }

  property("KDF > SCrypt > Encode and Decode") {
    // Decode test
    val testKdf = Helpers.ExpectedKdf.json.as[kdf.Kdf[Id]]
    testKdf.isRight shouldBe true
    testKdf shouldBe Right(Helpers.ExpectedKdf.value)
    // Encode test
    val testJson = Helpers.ExpectedKdf.value.asJson
    testJson.isNull shouldBe false
    testJson shouldBe Helpers.ExpectedKdf.json

    // Decode then Encode test
    val encodedFromDecoded = testKdf.map(_.asJson).toOption
    encodedFromDecoded.isDefined shouldBe true
    encodedFromDecoded.get.isNull shouldBe false
    encodedFromDecoded.get shouldBe Helpers.ExpectedKdf.json

    // Encode then Decode test
    val decodedFromEncoded = testJson.as[kdf.Kdf[Id]]
    decodedFromEncoded.isRight shouldBe true
    decodedFromEncoded shouldBe Right(Helpers.ExpectedKdf.value)
  }

  property("KDF > SCrypt > Decode fails with invalid label") {
    val fields = Helpers.ExpectedSCryptParams.fields // label is missing
    val invalidJson = Json.fromFields(fields)

    val testKdf = invalidJson.as[kdf.Kdf[Id]]
    testKdf.isLeft shouldBe true
    testKdf.swap.toOption.get shouldBe a[DecodingFailure]
  }

  property("KDF > SCrypt > Decode fails with invalid JSON") {
    // verify if underlying piece fails, the whole decode fails
    val fields = Helpers.ExpectedSCryptParams.fields.tail :+ ("kdf" -> Json.fromString(
      Helpers.ExpectedSCryptParams.value.kdf
    )) // salt is missing
    val invalidJson = Json.fromFields(fields)

    val testKdf = invalidJson.as[kdf.Kdf[Id]]
    testKdf.isLeft shouldBe true
    testKdf.swap.toOption.get shouldBe a[DecodingFailure]
  }

  property("VaultStore > Encode and Decode") {
    // Decode test
    val testVaultStore = Helpers.ExpectedVaultStore.json.as[VaultStore[Id]]
    testVaultStore.isRight shouldBe true
    testVaultStore shouldBe Right(Helpers.ExpectedVaultStore.value)
    // Encode test
    val testJson = Helpers.ExpectedVaultStore.value.asJson
    testJson.isNull shouldBe false
    testJson shouldBe Helpers.ExpectedVaultStore.json

    // Decode then Encode test
    val encodedFromDecoded = testVaultStore.map(_.asJson).toOption
    encodedFromDecoded.isDefined shouldBe true
    encodedFromDecoded.get.isNull shouldBe false
    encodedFromDecoded.get shouldBe Helpers.ExpectedVaultStore.json

    // Encode then Decode test
    val decodedFromEncoded = testJson.as[VaultStore[Id]]
    decodedFromEncoded.isRight shouldBe true
    decodedFromEncoded shouldBe Right(Helpers.ExpectedVaultStore.value)
  }

  property("VaultStore > Decode fails with invalid JSON") {
    // verify if underlying piece fails, the whole decode fails
    val invalidKdfParams = Helpers.ExpectedSCryptParams.fields.tail :+ ("kdf" -> Json.fromString(
      Helpers.ExpectedSCryptParams.value.kdf
    )) // salt is missing
    val fields = (
      "kdf" -> Json.fromFields(invalidKdfParams)
    ) +: Helpers.ExpectedVaultStore.fields.tail
    val invalidJson = Json.fromFields(fields)

    val testKdf = invalidJson.as[kdf.Kdf[Id]]
    testKdf.isLeft shouldBe true
    testKdf.swap.toOption.get shouldBe a[DecodingFailure]
  }

  property("VaultStore.fromJson utility behaves like json.as[VaultStore]") {
    val vaultStore1 = VaultStore.fromJson[Id](Helpers.ExpectedVaultStore.json)
    val vaultStore2 = Helpers.ExpectedVaultStore.json.as[VaultStore[Id]]
    vaultStore1 shouldBe vaultStore2
  }
}

object Helpers {

  object ExpectedAesParams {
    private val iv = "iv"
    val value: Aes.AesParams = Aes.AesParams(iv.getBytes)

    val fields: List[(String, Json)] = List(
      "iv" -> Json.fromString(iv)
    )
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedSCryptParams {
    private val salt = "salt"
    val value: SCrypt.SCryptParams = SCrypt.SCryptParams(salt.getBytes)

    val fields: List[(String, Json)] = List(
      "salt"  -> Json.fromString(salt),
      "n"     -> Json.fromInt(value.n),
      "r"     -> Json.fromInt(value.r),
      "p"     -> Json.fromInt(value.p),
      "dkLen" -> Json.fromInt(value.dkLen)
    )
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedCipher {
    val value: cipher.Cipher[Id] = Aes.make[Id](ExpectedAesParams.value)

    val fields: List[(String, Json)] =
      ExpectedAesParams.fields :+ ("cipher" -> Json.fromString(ExpectedAesParams.value.cipher))
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedKdf {
    val value: kdf.Kdf[Id] = SCrypt.make[Id](ExpectedSCryptParams.value)

    val fields: List[(String, Json)] =
      ExpectedSCryptParams.fields :+ ("kdf" -> Json.fromString(ExpectedSCryptParams.value.kdf))
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedVaultStore {
    private val cipherText = "cipherText"
    private val mac = "mac"
    val value: VaultStore[Id] = VaultStore(ExpectedKdf.value, ExpectedCipher.value, cipherText.getBytes, mac.getBytes)

    val fields: List[(String, Json)] = List(
      "kdf"        -> ExpectedKdf.json,
      "cipher"     -> ExpectedCipher.json,
      "cipherText" -> Json.fromString(cipherText),
      "mac"        -> Json.fromString(mac)
    )
    val json: Json = Json.fromFields(fields)
  }
}
