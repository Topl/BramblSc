package co.topl.brambl.common

import co.topl.brambl.models._
import co.topl.brambl.models.box.{Box, Lock, Value}
import co.topl.brambl.models.transaction._
import co.topl.quivr.Tokens
import com.google.protobuf.ByteString
import quivr.models._
import co.topl.brambl.common.ContainsImmutable.instances._

import java.nio.charset.StandardCharsets
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
    import ContainsImmutable.ContainsImmutableTOps

    implicit val immutableSignable: ContainsSignable[ImmutableBytes] = (t: ImmutableBytes) => SignableBytes(t.value)

    implicit val proofSignable: ContainsSignable[Proof] = _ => Array.emptyByteArray.immutable.signable
    implicit def immutableToSignable[T: ContainsImmutable](t: T): ContainsSignable[T] = _ => t.immutable.signable
  }
  object instances extends Instances
}
