package co.topl.crypto.encryption.kdf

import cats.Id
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SCryptSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("verify the same parameters (salt) and the same secret create the same key") {
    val params = SCrypt.SCryptParams(SCrypt.generateSalt)
    val sCrypt = SCrypt.make[Id](params)
    val secret = "secret".getBytes
    val derivedKey1 = sCrypt.deriveKey(secret)
    val derivedKey2 = sCrypt.deriveKey(secret)
    java.util.Arrays.equals(derivedKey1, derivedKey2) shouldBe true
  }

  property("verify different parameters (salt) for the same secret creates different keys") {
    val params1 = SCrypt.SCryptParams(SCrypt.generateSalt)
    var params2 = SCrypt.SCryptParams(SCrypt.generateSalt)
    while (params2.salt sameElements params1.salt)
      params2 = SCrypt.SCryptParams(SCrypt.generateSalt)
    val sCrypt1 = SCrypt.make[Id](params1)
    val sCrypt2 = SCrypt.make[Id](params2)
    val secret = "secret".getBytes
    val derivedKey1 = sCrypt1.deriveKey(secret)
    val derivedKey2 = sCrypt2.deriveKey(secret)
    java.util.Arrays.equals(derivedKey1, derivedKey2) should not be true
  }

  property("verify different secrets for the same parameters (salt) creates different keys") {
    val params = SCrypt.SCryptParams(SCrypt.generateSalt)
    val sCrypt = SCrypt.make[Id](params)
    val secret1 = "secret".getBytes
    val secret2 = "another-secret".getBytes.padTo(100, 0.toByte) // padding is arbitrary and shouldn't affect the result
    val derivedKey1 = sCrypt.deriveKey(secret1)
    val derivedKey2 = sCrypt.deriveKey(secret2)
    java.util.Arrays.equals(derivedKey1, derivedKey2) should not be true
  }
}
