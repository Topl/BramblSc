package co.topl.models

import co.topl.models.utility.{Lengths, Sized}
import scodec.bits.ByteVector

sealed trait Proof

object Proofs {

  /**
   * A proof which acts as a placeholder
   */
  case object Undefined extends Proof

  object Knowledge {
    case class Ed25519(bytes: ByteVector) extends Proof

    object Ed25519 {
      type Length = Lengths.bytes64.type
    }

    /**
     * This is not a zero-knowledge proof-of-knowledge
     */
    case class HashLock(value: Bytes) extends Proof
  }
}
