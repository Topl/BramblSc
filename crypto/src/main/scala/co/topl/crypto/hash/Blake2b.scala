package co.topl.crypto.hash

import co.topl.crypto.hash.digest.Digest
import org.bouncycastle.crypto.digests.Blake2bDigest

/**
 * Blake2b-* hashing scheme
 *
 * @tparam D the sized digest type with an implicit Digest implementation
 *
 * @note TODO This abstract class may no longer be needed; If it is needed, we should look into making it not be synchronized.
 */
abstract class Blake2bHash[D: Digest] extends Hash[Blake2b, D] {
  val digestSize: Int = Digest[D].size
  val digestSizeInBits: Int = 8 * digestSize
  lazy val digestFunc = new Blake2bDigest(digestSizeInBits)

  override def hash(prefix: Option[Byte], messages: Message*): D =
    // must be synchronized on the digest function so that everyone shares an instance
    synchronized {
      // update digest with prefix and messages
      prefix.foreach(p => digestFunc.update(p))
      messages.iterator.foreach { m =>
        digestFunc.update(m, 0, m.length)
      }

      val res = new Array[Byte](digestSize)

      // calling .doFinal resets to a default state
      digestFunc.doFinal(res, 0)

      Digest[D]
        .from(res)
        .valueOr(err => throw new Error(s"Blake2b hash with digest size $digestSize was invalid! $err"))
    }
}

/**
 * A 256 bit (32 byte) implementation of Blake2b
 *
 * @note this is not thread safe
 */
class Blake2b256 {
  private val digest = new Blake2bDigest(256)

  def hash(bytes: Array[Byte]*): Array[Byte] = {
    val out = new Array[Byte](32)
    bytes.foreach(b => digest.update(b, 0, b.length))
    digest.doFinal(out, 0)
    out
  }
}

/**
 * A 512 bit (64 byte) implementation of Blake2b
 *
 * @note this is not thread safe
 */
class Blake2b512 {
  private val digest = new Blake2bDigest(512)

  def hash(bytes: Array[Byte]*): Array[Byte] = {
    val out = new Array[Byte](64)
    bytes.foreach(b => digest.update(b, 0, b.length))
    digest.doFinal(out, 0)
    out
  }
}
