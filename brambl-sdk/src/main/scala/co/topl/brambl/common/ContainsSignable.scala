package co.topl.brambl.common

import quivr.models._
import ContainsImmutable.instances._
import ContainsImmutable._
import co.topl.brambl.models.transaction.{Attestation, IoTransaction, SpentTransactionOutput}

import scala.language.implicitConversions
// Long -> longSignable -> longSignableEvidence -> longSignableEvidenceId
// Long -> longSignable -> longSignableEvidence -> longSingableEvidenceSignable -> longSingableEvidenceSignableEvidence
// Object -> Signable -> Evidence -> Identifier -> Address -> KnownIdentifier

// Topl: TObject -> TSignable -> TEvidence -> TIdentifier -> TAddress -> TKnownIdentifier
// DAML: DObject -> DSignable -> DEvidence (==TEvidence) -> TSignable -> TEvidence -> TIdentifier -> TAddress -> TKnownIdentifier
trait ContainsSignable[T] {
  def signableBytes(t: T): SignableBytes
}

object ContainsSignable {
  def apply[T](implicit ev: ContainsSignable[T]): ContainsSignable[T] = ev

  implicit class ContainsSignableTOps[T: ContainsSignable](t: T) {
    def signable: SignableBytes = ContainsSignable[T].signableBytes(t)
  }

  trait Instances {
    implicit val immutableSignable: ContainsSignable[ImmutableBytes] = (t: ImmutableBytes) => SignableBytes(t.value)

    /**
     * Strips the proofs from a SpentTransactionOutput.
     * This is needed because the proofs are not part of the transaction's signable bytes
     */
    private def stripInput(stxo: SpentTransactionOutput): SpentTransactionOutput =
      stxo.copy(
        attestation = stxo.attestation.map(att =>
          att.copy(
            value = att.value match {
              case p: Attestation.Value.Predicate      => p.copy(p.value.copy(responses = Seq.empty))
              case i32: Attestation.Value.Image32      => i32.copy(i32.value.copy(responses = Seq.empty))
              case i64: Attestation.Value.Image64      => i64.copy(i64.value.copy(responses = Seq.empty))
              case c32: Attestation.Value.Commitment32 => c32.copy(c32.value.copy(responses = Seq.empty))
              case c64: Attestation.Value.Commitment64 => c64.copy(c64.value.copy(responses = Seq.empty))
            }
          )
        )
      )

    implicit val ioTransactionSignable: ContainsSignable[IoTransaction] = (iotx: IoTransaction) =>
      iotx
        .copy(
          inputs = iotx.inputs.map(stripInput),
          outputs = iotx.outputs,
          datum = iotx.datum
        )
        .immutable
        .signable

  }
  object instances extends Instances
}
