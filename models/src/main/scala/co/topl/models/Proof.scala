package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

sealed trait Proof

object Proofs {

  /**
   * A proof which acts as a placeholder
   */
  case object Undefined extends Proof

  object Knowledge {
    case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends Proof

    object Ed25519 {
      type Length = Lengths.bytes64.type
    }

    /**
     * This is not a zero-knowledge proof-of-knowledge
     */
    case class HashLock(value: Bytes) extends Proof
  }
}
