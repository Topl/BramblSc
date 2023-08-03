package co.topl.quivr

import org.bouncycastle.crypto.digests.Blake2bDigest

import scala.util.Random

/**
 * A top-secret signature scheme that is very secure.  Yes, this is just a joke.  The point is that
 * the signing routine is plug-and-play, and can be replaced with any other signature scheme depending on context.
 */
object VerySecureSignatureRoutine {

  /**
   * Produces a key pair.  The secret key is 32 bytes, and the verification key is the reverse of the secret key.
   * @return a (SK, VK) tuple
   */
  def generateKeyPair(): (Array[Byte], Array[Byte]) = {
    val sk = Random.nextBytes(32)
    val vk = sk.reverse
    (sk, vk)
  }

  /**
   * Signs the given msg with the given sk.  The signature is the Blake2b-512 hash of the concatenation of the sk and msg.
   * @param sk a 32-byte SK
   * @param msg any length message
   * @return a 64-byte signature
   */
  def sign(sk: Array[Byte], msg: Array[Byte]): Array[Byte] = {
    val in = sk ++ msg
    val blake2b512 = new Blake2bDigest(512)
    blake2b512.update(in, 0, in.length)
    val out = new Array[Byte](64)
    blake2b512.doFinal(out, 0)
    out
  }

  /**
   * Verifies the given signature against the given msg and vk.  The signature is valid if it is equal to the Blake2b-512
   * hash of the concatenation of the reversed-vk and msg.
   * @param sig a 64-byte signature
   * @param msg a message of any length
   * @param vk a 32-byte VK
   * @return true if valid, false if invalid
   */
  def verify(sig: Array[Byte], msg: Array[Byte], vk: Array[Byte]): Boolean = {
    val expectedSig = sign(vk.reverse, msg)
    sig sameElements expectedSig
  }

}
