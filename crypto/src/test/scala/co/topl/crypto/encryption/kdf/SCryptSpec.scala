package co.topl.crypto.encryption.kdf

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SCryptSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("verify the same parameters create the same key") {
    // Test with different sizes of secrets
  }
  property("verify changing the salt creates a different key") {}
}
