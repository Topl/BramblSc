package co.topl.crypto.encryption

import cats.Id
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.crypto.encryption.kdf.SCrypt
import co.topl.crypto.encryption.cipher.Aes

class VaultStoreSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  private def generateVaultStore(sensitiveInformation: Array[Byte], password: Array[Byte]) = {
    val kdfParams = SCrypt.SCryptParams(SCrypt.generateSalt)
    val kdf = SCrypt.make[Id](kdfParams)

    val cipherParams = Aes.AesParams(Aes.generateIv)
    val cipher = Aes.make[Id](cipherParams)

    val derivedKey = kdf.deriveKey(password)

    val cipherText = cipher.encrypt(sensitiveInformation, derivedKey)
    val mac = Mac.make(derivedKey, cipherText)

    VaultStore(kdf, cipher, cipherText, mac.value)
  }

  property("Verify decodeCipher produces the plain text secret") {
    val sensitiveInformation = "this is a secret".getBytes
    val password = "this is a password".getBytes
    val vaultStore = generateVaultStore(sensitiveInformation, password)

    val decoded = VaultStore.decodeCipher(vaultStore, password)

    decoded.isRight shouldBe true
    decoded.toOption.get shouldBe sensitiveInformation
  }

  property("Verify decodeCipher returns InvalidMac with a different password") {
    val sensitiveInformation = "this is a secret".getBytes
    val password = "this is a password".getBytes
    val vaultStore = generateVaultStore(sensitiveInformation, password)

    val decoded = VaultStore.decodeCipher(vaultStore, "this is a different password".getBytes)

    decoded.isLeft shouldBe true
    decoded.swap.toOption.get shouldBe VaultStore.InvalidMac
  }

  property("Verify decodeCipher returns InvalidMac with a corrupted VaultStore") {
    val sensitiveInformation = "this is a secret".getBytes
    val password = "this is a password".getBytes
    val vaultStore = generateVaultStore(sensitiveInformation, password)
    // VaultStore is corrupted by changing the cipher text
    val decoded1 =
      VaultStore.decodeCipher[Id](vaultStore.copy(cipherText = "this is an invalid cipher text".getBytes), password)
    decoded1.isLeft shouldBe true
    decoded1.swap.toOption.get shouldBe VaultStore.InvalidMac
    // VaultStore is corrupted by changing the mac
    val decoded2 = VaultStore.decodeCipher[Id](vaultStore.copy(mac = "this is an invalid mac".getBytes), password)
    decoded2.isLeft shouldBe true
    decoded2.swap.toOption.get shouldBe VaultStore.InvalidMac
    // VaultStore is corrupted by changing some parameter in KdfParams
    val kdfParams = SCrypt.SCryptParams("invalid salt".getBytes)
    val wrongKdf = SCrypt.make[Id](kdfParams)
    val decoded3 = VaultStore.decodeCipher[Id](vaultStore.copy(kdf = wrongKdf), password)
    decoded3.isLeft shouldBe true
    decoded3.swap.toOption.get shouldBe VaultStore.InvalidMac
  }
}
