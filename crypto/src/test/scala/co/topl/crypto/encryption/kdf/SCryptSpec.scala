package co.topl.crypto.encryption.kdf

import cats.Id
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SCryptSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("verify the same parameters (salt) and the same secret create the same key") {
    val params = Scrypt.ScryptParams[Id](Scrypt.generateSalt)
    val scrypt = Scrypt.make[Id]
    val secret = "secret".getBytes
    val derivedKey1 = scrypt.deriveKey(secret, params)
    val derivedKey2 = scrypt.deriveKey(secret, params)
    java.util.Arrays.equals(derivedKey1, derivedKey2) shouldBe true
  }

  property("verify different parameters (salt) for the same secret creates different keys") {
    val params1 = Scrypt.ScryptParams[Id](Scrypt.generateSalt)
    var params2 = Scrypt.ScryptParams[Id](Scrypt.generateSalt)
    while (params2.salt sameElements params1.salt)
      params2 = Scrypt.ScryptParams[Id](Scrypt.generateSalt)
    val scrypt = Scrypt.make[Id]
    val secret = "secret".getBytes
    val derivedKey1 = scrypt.deriveKey(secret, params1)
    val derivedKey2 = scrypt.deriveKey(secret, params2)
    java.util.Arrays.equals(derivedKey1, derivedKey2) should not be true
  }

  property("verify different secrets for the same parameters (salt) creates different keys") {
    val params = Scrypt.ScryptParams[Id](Scrypt.generateSalt)
    val scrypt = Scrypt.make[Id]
    val secret1 = "secret".getBytes
    val secret2 = "another-secret".getBytes.padTo(100, 0.toByte) // padding is arbitrary and shouldn't affect the result
    val derivedKey1 = scrypt.deriveKey(secret1, params)
    val derivedKey2 = scrypt.deriveKey(secret2, params)
    java.util.Arrays.equals(derivedKey1, derivedKey2) should not be true
  }
}
