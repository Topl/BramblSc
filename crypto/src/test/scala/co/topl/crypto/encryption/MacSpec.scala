package co.topl.crypto.encryption

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MacSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers{
  property("Different derived keys should produce different macs > Fail validation") {

  }
  property("Different cipher texts should produce different macs > Fail validation") {

  }
  property("Macs produced with the same derived key and the same cipher texts are identical > Pass validation") {

  }
}
