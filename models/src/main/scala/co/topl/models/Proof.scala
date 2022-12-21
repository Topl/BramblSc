package co.topl.models

import scodec.bits.ByteVector

sealed trait Proof

object Proofs {

  /**
   * A proof which acts as a placeholder
   */
  case object Undefined extends Proof

  object Knowledge {
    case class Ed25519(bytes: ByteVector) extends Proof

    /**
     * This is not a zero-knowledge proof-of-knowledge
     */
    case class HashLock(value: ByteVector) extends Proof
  }
}
