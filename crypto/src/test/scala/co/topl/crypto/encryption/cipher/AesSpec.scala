package co.topl.crypto.encryption.cipher

import cats.Id
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers

class AesSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("encrypting the same secret with different keys produces different ciphertexts") {
    val params = Aes.AesParams[Id](Aes.generateIv)
    val aes = Aes.make[Id]
    val encryptKey1 = "encryptKey1".getBytes.padTo(16, 0.toByte)
    val encryptKey2 = "encryptKey2".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText1 = aes.encrypt(message, encryptKey1, params)
    val cipherText2 = aes.encrypt(message, encryptKey2, params)
    java.util.Arrays.equals(cipherText1, cipherText2) should not be true
  }

  property("encrypting the same secret with different key lengths produces different ciphertexts") {
    val params = Aes.AesParams[Id](Aes.generateIv)
    val aes = Aes.make[Id]
    val encryptKey1 = "encryptKey".getBytes.padTo(16, 0.toByte)
    val encryptKey2 = "encryptKey".getBytes.padTo(32, 0.toByte)
    val message = "message".getBytes
    val cipherText1 = aes.encrypt(message, encryptKey1, params)
    val cipherText2 = aes.encrypt(message, encryptKey2, params)
    java.util.Arrays.equals(cipherText1, cipherText2) should not be true
  }

  property("encrypting the same secret with different ivs produces different ciphertexts") {
    val params1 = Aes.AesParams[Id](Aes.generateIv)
    var params2 = Aes.AesParams[Id](Aes.generateIv)
    while (params2.iv sameElements params1.iv)
      params2 = Aes.AesParams[Id](Aes.generateIv)
    val aes = Aes.make[Id]
    val key = "key".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText1 = aes.encrypt(message, key, params1)
    val cipherText2 = aes.encrypt(message, key, params2)
    java.util.Arrays.equals(cipherText1, cipherText2) should not be true
  }

  property("encrypt and decrypt is successful with the same key and iv") {
    val aes = Aes.make[Id]
    // Test with different sizes of keys
    List(16, 24, 32).map("key".getBytes.padTo(_, 0.toByte)).foreach { key =>
      val params = Aes.AesParams[Id](Aes.generateIv)
      val message = "message".getBytes
      val cipherText = aes.encrypt(message, key, params)
      val decodedText = aes.decrypt(cipherText, key, params)
      java.util.Arrays.equals(decodedText, message) shouldBe true
    }
  }

  property("encrypt and decrypt is successful with different sizes of messages") {
    val aes = Aes.make[Id]
    // The purpose is to test the padding of the message (to be a multiple of 16) and the removal of the padding when
    // decrypting. We should test with different sizes of messages to ensure the padding is done correctly.
    List(
      7, // arbitrary < 16
      Aes.BlockSize - 1, // 1 less than a block
      Aes.BlockSize, // a full block
      Aes.BlockSize + 1, // 1 more than a block
      24, // arbitrary > 16 and < 32
      (Aes.BlockSize * 2) - 1, // 1 less than 2 blocks
      Aes.BlockSize * 2, // a multiple of a block (i.e, 2 blocks)
      (Aes.BlockSize * 2) + 1 // 1 more than 2 blocks
    ).map("message".getBytes.padTo(_, 0.toByte)).foreach { message =>
      val params = Aes.AesParams[Id](Aes.generateIv)
      val key = "key".getBytes.padTo(16, 0.toByte)
      val cipherText = aes.encrypt(message, key, params)
      val decodedText = aes.decrypt(cipherText, key, params)
      java.util.Arrays.equals(decodedText, message) shouldBe true
    }
  }

  property("encrypt and decrypt is unsuccessful with a different key") {
    val params = Aes.AesParams[Id](Aes.generateIv)
    val aes = Aes.make[Id]
    val encryptKey = "encryptKey".getBytes.padTo(16, 0.toByte)
    val decryptKey = "decryptKey".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText = aes.encrypt(message, encryptKey, params)
    val decodedText = aes.decrypt(cipherText, decryptKey, params)
    java.util.Arrays.equals(decodedText, message) should not be true
  }

  property("encrypt and decrypt is unsuccessful with a different iv") {
    val encryptParams = Aes.AesParams[Id](Aes.generateIv)
    var decryptParams = Aes.AesParams[Id](Aes.generateIv)
    val aes = Aes.make[Id]
    while (decryptParams.iv sameElements encryptParams.iv)
      decryptParams = Aes.AesParams[Id](Aes.generateIv)
    val key = "key".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText = aes.encrypt(message, key, encryptParams)
    val decodedText = aes.decrypt(cipherText, key, decryptParams)

    java.util.Arrays.equals(decodedText, message) should not be true
  }
}
