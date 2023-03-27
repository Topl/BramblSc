package co.topl.crypto.encryption.cipher

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers

class AesSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("encrypting the same secret with different keys produces different ciphertexts") {
    val params = AesParams(Aes.generateIv)
    val encryptKey1 = "encryptKey1".getBytes.padTo(16, 0.toByte)
    val encryptKey2 = "encryptKey2".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText1 = Aes.encrypt(message, encryptKey1, params)
    val cipherText2 = Aes.encrypt(message, encryptKey2, params)
    (cipherText1 sameElements cipherText2) should not be true
  }

  property("encrypting the same secret with different key lengths produces different ciphertexts") {
    val params = AesParams(Aes.generateIv)
    val encryptKey1 = "encryptKey".getBytes.padTo(16, 0.toByte)
    val encryptKey2 = "encryptKey".getBytes.padTo(32, 0.toByte)
    val message = "message".getBytes
    val cipherText1 = Aes.encrypt(message, encryptKey1, params)
    val cipherText2 = Aes.encrypt(message, encryptKey2, params)
    (cipherText1 sameElements cipherText2) should not be true
  }

  property("encrypting the same secret with different ivs produces different ciphertexts") {
    val params1 = AesParams(Aes.generateIv)
    var params2 = AesParams(Aes.generateIv)
    while (params2.iv sameElements params1.iv)
      params2 = AesParams(Aes.generateIv)
    val key = "key".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText1 = Aes.encrypt(message, key, params1)
    val cipherText2 = Aes.encrypt(message, key, params2)
    (cipherText1 sameElements cipherText2) should not be true
  }

  property("encrypt and decrypt is successful with the same key and iv") {
    // Test with different sizes of keys
    List(16, 24, 32).map("key".getBytes.padTo(_, 0.toByte)).foreach { key =>
      val params = AesParams(Aes.generateIv)
      val message = "message".getBytes
      val cipherText = Aes.encrypt(message, key, params)
      val decodedText = Aes.decrypt(cipherText, key, params)
      (decodedText sameElements message) shouldBe true
    }
  }

  property("encrypt and decrypt is successful with different sizes of messages") {
    // The purpose is to test the padding of the message (to be a multiple of 16) and the removal of the padding when
    // decrypting. We should test with different sizes of messages to ensure the padding is done correctly.
    List(
      7, // arbitrary < 16
      15, // 1 less than 16
      16, // a full block
      17, // 1 more than 16
      24, // arbitrary > 16 and < 32
      31, // 1 less than 32
      32, // a full multiple of a full block
      33 // 1 more than 32
    ).map("message".getBytes.padTo(_, 0.toByte)).foreach { message =>
      val params = AesParams(Aes.generateIv)
      val key = "key".getBytes.padTo(16, 0.toByte)
      val cipherText = Aes.encrypt(message, key, params)
      val decodedText = Aes.decrypt(cipherText, key, params)
      (decodedText sameElements message) shouldBe true
    }
  }

  property("encrypt and decrypt is unsuccessful with a different key") {
    val params = AesParams(Aes.generateIv)
    val encryptKey = "encryptKey".getBytes.padTo(16, 0.toByte)
    val decryptKey = "decryptKey".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText = Aes.encrypt(message, encryptKey, params)
    val decodedText = Aes.decrypt(cipherText, decryptKey, params)
    (decodedText sameElements message) should not be true
  }

  property("encrypt and decrypt is unsuccessful with a different iv") {
    val encryptParams = AesParams(Aes.generateIv)
    var decryptParams = AesParams(Aes.generateIv)
    while (decryptParams.iv sameElements encryptParams.iv)
      decryptParams = AesParams(Aes.generateIv)
    val key = "key".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText = Aes.encrypt(message, key, encryptParams)
    val decodedText = Aes.decrypt(cipherText, key, decryptParams)

    (decodedText sameElements message) should not be true
  }
}
