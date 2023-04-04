package co.topl.crypto.encryption

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class VaultStoreSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  property("Verify decodeCipher produces the plain text secret") {}
  property("Verify decodeCipher returns InvalidMac with a different password") {}

  property("Verify decodeCipher returns InvalidMac with a corrupted VaultStore") {
    // VaultStore is corrupted by changing the cipher text
    // VaultStore is corrupted by changing the mac
    // VaultStore is corrupted by changing some parameter in KdfParams
    // VaultStore is corrupted by changing some parameter in CipherParams
  }
}
