package co.topl.crypto.encryption.cipher

import cats.Id
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers

class AesSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("encrypting the same secret with different keys produces different ciphertexts") {
    val params = Aes.AesParams(Aes.generateIv)
    val aes = Aes.make[Id](params)
    val encryptKey1 = "encryptKey1".getBytes.padTo(16, 0.toByte)
    val encryptKey2 = "encryptKey2".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText1 = aes.encrypt(message, encryptKey1)
    val cipherText2 = aes.encrypt(message, encryptKey2)
    java.util.Arrays.equals(cipherText1, cipherText2) should not be true
  }

  property("encrypting the same secret with different key lengths produces different ciphertexts") {
    val params = Aes.AesParams(Aes.generateIv)
    val aes = Aes.make[Id](params)
    val encryptKey1 = "encryptKey".getBytes.padTo(16, 0.toByte)
    val encryptKey2 = "encryptKey".getBytes.padTo(32, 0.toByte)
    val message = "message".getBytes
    val cipherText1 = aes.encrypt(message, encryptKey1)
    val cipherText2 = aes.encrypt(message, encryptKey2)
    java.util.Arrays.equals(cipherText1, cipherText2) should not be true
  }

  property("encrypting the same secret with different ivs produces different ciphertexts") {
    val params1 = Aes.AesParams(Aes.generateIv)
    var params2 = Aes.AesParams(Aes.generateIv)
    while (params2.iv sameElements params1.iv)
      params2 = Aes.AesParams(Aes.generateIv)
    val aes1 = Aes.make[Id](params1)
    val aes2 = Aes.make[Id](params2)
    val key = "key".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText1 = aes1.encrypt(message, key)
    val cipherText2 = aes2.encrypt(message, key)
    java.util.Arrays.equals(cipherText1, cipherText2) should not be true
  }

  property("encrypt and decrypt is successful with the same key and iv") {
    // Test with different sizes of keys
    List(16, 24, 32).map("key".getBytes.padTo(_, 0.toByte)).foreach { key =>
      val params = Aes.AesParams(Aes.generateIv)
      val aes = Aes.make[Id](params)
      val message = "message".getBytes
      val cipherText = aes.encrypt(message, key)
      val decodedText = aes.decrypt(cipherText, key)
      java.util.Arrays.equals(decodedText, message) shouldBe true
    }
  }

  property("encrypt and decrypt is successful with different sizes of messages") {
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
      val params = Aes.AesParams(Aes.generateIv)
      val aes = Aes.make[Id](params)
      val key = "key".getBytes.padTo(16, 0.toByte)
      val cipherText = aes.encrypt(message, key)
      val decodedText = aes.decrypt(cipherText, key)
      java.util.Arrays.equals(decodedText, message) shouldBe true
    }
  }

  property("encrypt and decrypt is unsuccessful with a different key") {
    val params = Aes.AesParams(Aes.generateIv)
    val aes = Aes.make[Id](params)
    val encryptKey = "encryptKey".getBytes.padTo(16, 0.toByte)
    val decryptKey = "decryptKey".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText = aes.encrypt(message, encryptKey)
    val decodedText = aes.decrypt(cipherText, decryptKey)
    java.util.Arrays.equals(decodedText, message) should not be true
  }

  property("encrypt and decrypt is unsuccessful with a different iv") {
    val encryptParams = Aes.AesParams(Aes.generateIv)
    var decryptParams = Aes.AesParams(Aes.generateIv)
    while (decryptParams.iv sameElements encryptParams.iv)
      decryptParams = Aes.AesParams(Aes.generateIv)
    val aesEncrypt = Aes.make[Id](encryptParams)
    val aesDecrypt = Aes.make[Id](decryptParams)
    val key = "key".getBytes.padTo(16, 0.toByte)
    val message = "message".getBytes
    val cipherText = aesEncrypt.encrypt(message, key)
    val decodedText = aesDecrypt.decrypt(cipherText, key)

    java.util.Arrays.equals(decodedText, message) should not be true
  }
}
