package co.topl.brambl.common

import co.topl.brambl.models.Evidence
import co.topl.crypto.accumulators.LeafData
import co.topl.crypto.accumulators.merkle.MerkleTree
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.implicits._
import co.topl.crypto.hash.Blake2b
import co.topl.crypto.hash.blake2b256
import com.google.protobuf.ByteString
import quivr.models.Digest

/**
 * Contains signable bytes and has methods to get evidence of those bytes in the form of a 32 or 64 byte hash.
 */
trait ContainsEvidence[T] {
  def sizedEvidence(t: T): Evidence
}

object ContainsEvidence {
  def apply[T](implicit ev: ContainsEvidence[T]): ContainsEvidence[T] = ev

  implicit class Ops[T: ContainsEvidence](t: T) {
    def sizedEvidence: Evidence = ContainsEvidence[T].sizedEvidence(t)

  }

  implicit def blake2bEvidenceFromImmutable[T: ContainsImmutable]: ContainsEvidence[T] =
    new ContainsEvidence[T] {

      override def sizedEvidence(t: T): Evidence =
        Evidence(
          Digest(
            ByteString.copyFrom(
              blake2b256
                .hash(
                  ContainsImmutable[T].immutableBytes(t).value.toByteArray
                )
                .value
            )
          )
        )
    }

  implicit def merkleRootFromBlake2bEvidence[T: ContainsImmutable]: ContainsEvidence[List[T]] =
    new ContainsEvidence[List[T]] {

      override def sizedEvidence(list: List[T]): Evidence =
        Evidence(
          Digest(
            ByteString.copyFrom(
              MerkleTree
                .apply[Blake2b, Digest32](
                  list.zipWithIndex
                    .map { case (item, index) =>
                      LeafData(ContainsImmutable[T].immutableBytes(item).value.toByteArray)
                    }
                )
                .rootHash
                .value
            )
          )
        )
    }
}
