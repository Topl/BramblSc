package co.topl.brambl.common

import ContainsImmutable._
import ContainsImmutable.instances._
import co.topl.brambl.models.box.Attestation
import co.topl.brambl.models.common.ImmutableBytes
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.SpentTransactionOutput
import quivr.models._

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
        attestation = stxo.attestation.copy(
          value = stxo.attestation.value match {
            case p: Attestation.Value.Predicate  => p.copy(p.value.copy(responses = Seq.empty))
            case i: Attestation.Value.Image      => i.copy(i.value.copy(responses = Seq.empty))
            case c: Attestation.Value.Commitment => c.copy(c.value.copy(responses = Seq.empty))
            case e                               => e
          }
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
