package co.topl.crypto

package object signing {
  type Bytes = Array[Byte]
  type Message = Bytes

  trait SigningKey
  trait VerificationKey
  case class KeyPair(signingKey: SigningKey, verificationKey: VerificationKey)

  abstract class SizedSignature(size: Int) {
    val bytes: Bytes
    require(
      bytes.length == size,
      s"Invalid signature length. Expected: ${size}, Received: ${bytes.length}"
    )
  }

  abstract class SizedSeed(size: Int){
    val bytes: Bytes
    require(
      bytes.length == size,
      s"Invalid seed length. Expected: ${size}, Received: ${bytes.length}"
    )
  }
}
