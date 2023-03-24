package co.topl.crypto.encryption.cipher

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers

class AesSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("encrypt and decrypt is successful with the same key and iv") {
    // Test with different sizes of plaintext, keys, and ivs
  }
  property("encrypt and decrypt is unsuccessful with a different key") {}
  property("encrypt and decrypt is unsuccessful with a different iv") {}
  property("encrypting the same secret with different keys and iv produces different ciphertexts") {}
}
