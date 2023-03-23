package co.topl.crypto.encryption.kdf

/**
 * Scrypt is a key derivation function.
 * @see [[https://en.wikipedia.org/wiki/Scrypt]]
 */
class Scrypt extends Kdf[Scrypt.ScryptParams] {
  override def deriveKey(secret: Array[Byte], params: Scrypt.ScryptParams): Array[Byte] = ???
}

object Scrypt {
  trait ScryptParams extends Params {
    val salt : Array[Byte] // salt
    val n: Int // CPU/memory cost parameter
    val r: Int // block size parameter
    val p: Int // parallelization parameter
    val dkLen: Int // length of derived key
  }
}
