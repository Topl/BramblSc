package co.topl.brambl.common

import cats.implicits._
import co.topl.brambl.models.Evidence
import co.topl.crypto.accumulators.LeafData
import co.topl.crypto.accumulators.merkle.MerkleTree
import co.topl.crypto.hash.digest.{Digest32, Digest64}
import co.topl.crypto.hash.implicits._
import co.topl.crypto.hash.{blake2b256, blake2b512, Blake2b}
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

  implicit def blake2bEvidenceFromImmutable[T: ContainsImmutable]: ContainsEvidence[T] =
    new ContainsEvidence[T] {

      override def sized32Evidence(t: T): Evidence.Sized32 =
        Evidence.Sized32(
          Digest
            .Digest32(
              ByteString.copyFrom(
                blake2b256
                  .hash(
                    ContainsImmutable[T].immutableBytes(t).value.toByteArray
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
                    ContainsImmutable[T].immutableBytes(t).value.toByteArray
                  )
                  .value
              )
            )
            .some
        )
    }

  implicit def merkleRootFromBlake2bEvidence[T: ContainsImmutable]: ContainsEvidence[List[T]] =
    new ContainsEvidence[List[T]] {

      override def sized32Evidence(list: List[T]): Evidence.Sized32 =
        Evidence.Sized32(
          Digest
            .Digest32(
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
                      .map { case (item, index) =>
                        LeafData(ContainsImmutable[T].immutableBytes(item).value.toByteArray)
                      }
                  )
                  .rootHash
                  .value
              )
            )
            .some
        )
    }
}
