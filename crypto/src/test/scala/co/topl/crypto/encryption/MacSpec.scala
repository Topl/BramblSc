package co.topl.crypto.encryption

import cats.Id
import co.topl.crypto.utils.randomBytes
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MacSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("Different derived keys should produce different macs > Fail validation") {
    val dKey1 = randomBytes()
    var dKey2 = randomBytes()
    while (java.util.Arrays.equals(dKey1, dKey2))
      dKey2 = randomBytes()
    val ciphertext = "ciphertext".getBytes
    val mac1 = Mac.make(dKey1, ciphertext)
    val mac2 = Mac.make(dKey2, ciphertext)
    mac1.validateMac[Id](mac2) shouldBe false
    mac2.validateMac[Id](mac1) shouldBe false
  }

  property("Different cipher texts should produce different macs > Fail validation") {
    val dKey = randomBytes()
    val ciphertext1 = "ciphertext1".getBytes
    val ciphertext2 = "ciphertext2".getBytes
    val mac1 = Mac.make(dKey, ciphertext1)
    val mac2 = Mac.make(dKey, ciphertext2)
    mac1.validateMac[Id](mac2) shouldBe false
    mac2.validateMac[Id](mac1) shouldBe false
  }

  property("Macs produced with the same derived key and the same cipher texts are identical > Pass validation") {
    val dKey = randomBytes()
    val ciphertext = "ciphertext".getBytes
    val mac1 = Mac.make(dKey, ciphertext)
    val mac2 = Mac.make(dKey, ciphertext)
    mac1.validateMac[Id](mac2) shouldBe true
    mac2.validateMac[Id](mac1) shouldBe true
  }
}
