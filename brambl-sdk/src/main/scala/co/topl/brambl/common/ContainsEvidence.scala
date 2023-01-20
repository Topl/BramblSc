package co.topl.brambl.common

import cats.implicits._
import co.topl.brambl.models.Evidence
import co.topl.crypto.accumulators.LeafData
import co.topl.crypto.accumulators.merkle.MerkleTree
import co.topl.crypto.hash.digest.{Digest32, Digest64}
import co.topl.crypto.hash.{Blake2b, blake2b256, blake2b512}
import co.topl.crypto.implicits.{blake2b256Hash, blake2b512Hash, digestDigest32, digestDigest64}
import com.google.protobuf.ByteString
import quivr.models.Digest

/**
 * Contains signable bytes and has methods to get evidence of those bytes in the form of a 32 or 64 byte hash.
 */
trait ContainsEvidence[T] {
  def sized32Evidence(t: T): Evidence.Sized32
  def sized64Evidence(t: T): Evidence.Sized64
}

object ContainsEvidence {
  def apply[T](implicit ev: ContainsEvidence[T]): ContainsEvidence[T] = ev

  implicit class Ops[T: ContainsEvidence](t: T) {
    def sized32Evidence: Evidence.Sized32 = ContainsEvidence[T].sized32Evidence(t)

    def sized64Evidence: Evidence.Sized64 = ContainsEvidence[T].sized64Evidence(t)
  }

  implicit def blake2bEvidenceFromSignable[T: ContainsSignable]: ContainsEvidence[T] =
    new ContainsEvidence[T] {

      override def sized32Evidence(t: T): Evidence.Sized32 =
        Evidence.Sized32(
          Digest
            .Digest32(
              ByteString.copyFrom(
                blake2b256
                  .hash(
                    ContainsSignable[T].signableBytes(t).value.toByteArray
                  )
                  .value
              )
            )
            .some
        )

      override def sized64Evidence(t: T): Evidence.Sized64 =
        Evidence.Sized64(
          Digest
            .Digest64(
              ByteString.copyFrom(
                blake2b512
                  .hash(
                    ContainsSignable[T].signableBytes(t).value.toByteArray
                  )
                  .value
              )
            )
            .some
        )
    }

  implicit def merkleRootFromBlake2bEvidence[T: ContainsSignable]: ContainsEvidence[List[T]] =
    new ContainsEvidence[List[T]] {

      override def sized32Evidence(list: List[T]): Evidence.Sized32 =
        Evidence.Sized32(
          Digest
            .Digest32(
              ByteString.copyFrom(
                MerkleTree
                  .apply[Blake2b, Digest32](
                    list.zipWithIndex
                      .map { case (item, index) => LeafData(ContainsSignable[T].signableBytes(item).value.toByteArray) }
                  )
                  .rootHash
                  .value
              )
            )
            .some
        )

      override def sized64Evidence(list: List[T]): Evidence.Sized64 =
        Evidence.Sized64(
          Digest
            .Digest64(
              ByteString.copyFrom(
                MerkleTree
                  .apply[Blake2b, Digest64](
                    list.zipWithIndex
                      .map { case (item, index) => LeafData(ContainsSignable[T].signableBytes(item).value.toByteArray) }
                  )
                  .rootHash
                  .value
              )
            )
            .some
        )
    }
}
