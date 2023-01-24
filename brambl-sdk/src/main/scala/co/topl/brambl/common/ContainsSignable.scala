package co.topl.brambl.common

import co.topl.brambl.models._
import co.topl.brambl.models.box.{Box, Lock, Value}
import co.topl.brambl.models.transaction._
import co.topl.quivr.Tokens
import com.google.protobuf.ByteString
import quivr.models._

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

  implicit class SignableOps(val t: SignableBytes) extends AnyVal {
    def ++(other: SignableBytes): SignableBytes = SignableBytes(t.value.concat(other.value))
  }

  trait Instances {

    implicit private def arrayByteToSignableBytes(bytes: Array[Byte]): SignableBytes =
      SignableBytes(ByteString.copyFrom(bytes))

    implicit val byteSingable: ContainsSignable[Byte] = (t: Byte) => Array(t)

    implicit val arrayByteSignable: ContainsSignable[Array[Byte]] = (bytes: Array[Byte]) => bytes

    implicit val byteStringSignable: ContainsSignable[ByteString] = _.toByteArray.signable

    implicit val intSignable: ContainsSignable[Int] = (int: Int) => BigInt(int).toByteArray

    implicit val longSignable: ContainsSignable[Long] = (long: Long) => BigInt(long).toByteArray

    implicit val stringSignable: ContainsSignable[String] = (string: String) => string.getBytes(StandardCharsets.UTF_8)

    implicit def seqSignable[T: ContainsSignable]: ContainsSignable[Seq[T]] = (seq: Seq[T]) =>
      seq.zipWithIndex.foldLeft(SignableBytes()) { case (acc, (item, index)) =>
        acc ++
        index.signable ++
        item.signable
      }

    implicit def optionSignable[T: ContainsSignable]: ContainsSignable[Option[T]] = (option: Option[T]) =>
      option.fold(Array(0xff.toByte): SignableBytes)(_.signable)

    implicit val int128Signable: ContainsSignable[Int128] = _.value.signable

    implicit val smallDataSignable: ContainsSignable[SmallData] = _.value.signable

    implicit val rootSignable: ContainsSignable[Root] = _.value match {
      case Root.Value.Root32(v) => v.signable
      case Root.Value.Root64(v) => v.signable
    }

    implicit val verificationKeySignable: ContainsSignable[VerificationKey] = _.value.signable

    implicit val datumSignable: ContainsSignable[Datum] = _.value match {
      case Datum.Value.Eon(v)           => v.signable
      case Datum.Value.Era(v)           => v.signable
      case Datum.Value.Epoch(v)         => v.signable
      case Datum.Value.Header(v)        => v.signable
      case Datum.Value.Root(v)          => v.signable
      case Datum.Value.IoTransaction(v) => v.signable
      case Datum.Value.SpentOutput(v)   => v.signable
      case Datum.Value.UnspentOutput(v) => v.signable
    }

    implicit val eonDatumSignable: ContainsSignable[Datum.Eon] = _.event.signable
    implicit val eraDatumSignable: ContainsSignable[Datum.Era] = _.event.signable
    implicit val epochDatumSignable: ContainsSignable[Datum.Epoch] = _.event.signable
    implicit val headerDatumSignable: ContainsSignable[Datum.Header] = _.event.signable
    implicit val rootDatumSignable: ContainsSignable[Datum.Root] = _.event.signable
    implicit val ioTransactionDatumSignable: ContainsSignable[Datum.IoTransaction] = _.event.signable
    implicit val spentOutputDatumSignable: ContainsSignable[Datum.SpentOutput] = _.event.signable
    implicit val unspentOutputDatumSignable: ContainsSignable[Datum.UnspentOutput] = _.event.signable

    implicit val ioTransactionSignable: ContainsSignable[IoTransaction] = (iotx: IoTransaction) =>
      iotx.inputs.signable ++
      iotx.outputs.signable ++
      iotx.datum.signable

    implicit val iotxScheduleSignable: ContainsSignable[Schedule] = (schedule: Schedule) =>
      schedule.min.signable ++
      schedule.max.signable

    implicit val spentOutputSignable: ContainsSignable[SpentTransactionOutput] = (stxo: SpentTransactionOutput) =>
      stxo.knownIdentifier.signable ++
      stxo.attestation.signable ++
      stxo.value.signable ++
      stxo.datum.signable

    implicit val unspentOutputSignable: ContainsSignable[UnspentTransactionOutput] = (utxo: UnspentTransactionOutput) =>
      utxo.address.signable ++
      utxo.value.signable ++
      utxo.datum.signable

    implicit val boxSignable: ContainsSignable[Box] = (box: Box) =>
      box.lock.signable ++
      box.value.signable

    implicit val valueSignable: ContainsSignable[Value] = _.value match {
      case Value.Value.Token(v) => v.signable
      case Value.Value.Asset(v) => v.signable
    }

    implicit val addressSignable: ContainsSignable[Address] = (address: Address) =>
      address.network.signable ++
      address.ledger.signable ++
      address.identifier.signable

    implicit val size32EvidenceSignable: ContainsSignable[Evidence.Sized32] =
      ev => ev.digest.signable

    implicit val size64EvidenceSignable: ContainsSignable[Evidence.Sized64] =
      ev => ev.digest.signable

    implicit val evidenceSignable: ContainsSignable[Evidence] = _.value match {
      case Evidence.Value.Sized32(e) => size32EvidenceSignable.signableBytes(e)
      case Evidence.Value.Sized64(e) => size64EvidenceSignable.signableBytes(e)
    }

    implicit val digest32Signable: ContainsSignable[Digest.Digest32] = _.value.signable
    implicit val digest64Signable: ContainsSignable[Digest.Digest64] = _.value.signable

    implicit val digestSignable: ContainsSignable[Digest] = _.value match {
      case Digest.Value.Digest32(v) => v.signable
      case Digest.Value.Digest64(v) => v.signable
    }

    implicit val accumulatorRoot32IdentifierSignable: ContainsSignable[Identifier.AccumulatorRoot32] =
      id =>
        Tags.Identifier.AccumulatorRoot32.signable ++
        id.evidence.signable

    implicit val accumulatorRoot64IdentifierSignable: ContainsSignable[Identifier.AccumulatorRoot64] =
      id =>
        Tags.Identifier.AccumulatorRoot64.signable ++
        id.evidence.signable

    implicit val boxLock32IdentifierSignable: ContainsSignable[Identifier.Lock32] =
      id =>
        Tags.Identifier.Lock32.signable ++
        id.evidence.signable

    implicit val boxLock64IdentifierSignable: ContainsSignable[Identifier.Lock64] =
      id =>
        Tags.Identifier.Lock64.signable ++
        id.evidence.signable

    implicit val boxValue32IdentifierSignable: ContainsSignable[Identifier.BoxValue32] =
      id =>
        Tags.Identifier.BoxValue32.signable ++
        id.evidence.signable

    implicit val boxValue64IdentifierSignable: ContainsSignable[Identifier.BoxValue64] =
      id =>
        Tags.Identifier.BoxValue64.signable ++
        id.evidence.signable

    implicit val ioTransaction32IdentifierSignable: ContainsSignable[Identifier.IoTransaction32] =
      id =>
        Tags.Identifier.IoTransaction32.signable ++
        id.evidence.signable

    implicit val ioTransaction64IdentifierSignable: ContainsSignable[Identifier.IoTransaction64] =
      id =>
        Tags.Identifier.IoTransaction64.signable ++
        id.evidence.signable

    implicit val identifiersSignable: ContainsSignable[Identifier] = _.value match {
      case Identifier.Value.AccumulatorRoot32(i) => accumulatorRoot32IdentifierSignable.signableBytes(i)
      case Identifier.Value.AccumulatorRoot64(i) => accumulatorRoot64IdentifierSignable.signableBytes(i)
      case Identifier.Value.Lock32(i)            => boxLock32IdentifierSignable.signableBytes(i)
      case Identifier.Value.Lock64(i)            => boxLock64IdentifierSignable.signableBytes(i)
      case Identifier.Value.BoxValue32(i)        => boxValue32IdentifierSignable.signableBytes(i)
      case Identifier.Value.BoxValue64(i)        => boxValue64IdentifierSignable.signableBytes(i)
      case Identifier.Value.IoTransaction32(i)   => ioTransaction32IdentifierSignable.signableBytes(i)
      case Identifier.Value.IoTransaction64(i)   => ioTransaction64IdentifierSignable.signableBytes(i)
    }

    implicit val knownOutput32IdentifierSignable: ContainsSignable[KnownIdentifier.TransactionOutput32] =
      (knownId: KnownIdentifier.TransactionOutput32) =>
        knownId.network.signable ++
        knownId.ledger.signable ++
        knownId.index.signable ++
        knownId.id.signable

    implicit val knownOutput64IdentifierSignable: ContainsSignable[KnownIdentifier.TransactionOutput64] =
      (knownId: KnownIdentifier.TransactionOutput64) =>
        knownId.network.signable ++
        knownId.ledger.signable ++
        knownId.index.signable ++
        knownId.id.signable

    implicit val knownIdentifierSignable: ContainsSignable[KnownIdentifier] = _.value match {
      case KnownIdentifier.Value.TransactionOutput32(r) => knownOutput32IdentifierSignable.signableBytes(r)
      case KnownIdentifier.Value.TransactionOutput64(r) => knownOutput64IdentifierSignable.signableBytes(r)
    }

    implicit val tokenValueSignable: ContainsSignable[Value.Token] = (token: Value.Token) => token.quantity.signable

    implicit val assetValueSignable: ContainsSignable[Value.Asset] = (asset: Value.Asset) =>
      asset.label.signable ++
      asset.quantity.signable ++
      asset.metadata.signable

    // consider making predicate non-empty
    implicit val predicateLockSignable: ContainsSignable[Lock.Predicate] = (predicate: Lock.Predicate) =>
      predicate.threshold.signable ++
      predicate.challenges.signable

    implicit val image32LockSignable: ContainsSignable[Lock.Image32] = (image: Lock.Image32) =>
      image.threshold.signable ++
      image.leaves.signable

    implicit val image64LockSignable: ContainsSignable[Lock.Image64] = (image: Lock.Image64) =>
      image.threshold.signable ++
      image.leaves.signable

    implicit val commitment32LockSignable: ContainsSignable[Lock.Commitment32] = (commitment: Lock.Commitment32) =>
      commitment.threshold.signable ++
      commitment.root.size.signable ++
      commitment.root.signable

    implicit val commitment64LockSignable: ContainsSignable[Lock.Commitment64] = (commitment: Lock.Commitment64) =>
      commitment.threshold.signable ++
      commitment.root.size.signable ++
      commitment.root.signable

    implicit val lockSignable: ContainsSignable[Lock] = _.value match {
      case Lock.Value.Predicate(l)    => predicateLockSignable.signableBytes(l)
      case Lock.Value.Image32(l)      => image32LockSignable.signableBytes(l)
      case Lock.Value.Image64(l)      => image64LockSignable.signableBytes(l)
      case Lock.Value.Commitment32(l) => commitment32LockSignable.signableBytes(l)
      case Lock.Value.Commitment64(l) => commitment64LockSignable.signableBytes(l)
    }

    implicit val predicateAttestationSignable: ContainsSignable[Attestation.Predicate] =
      attestation =>
        attestation.lock.signable ++
        attestation.responses.signable

    implicit val image32AttestationSignable: ContainsSignable[Attestation.Image32] =
      attestation =>
        attestation.lock.signable ++
        attestation.known.signable ++
        attestation.responses.signable

    implicit val image64AttestationSignable: ContainsSignable[Attestation.Image64] =
      attestation =>
        attestation.lock.signable ++
        attestation.known.signable ++
        attestation.responses.signable

    implicit val commitment32AttestationSignable: ContainsSignable[Attestation.Commitment32] =
      attestation =>
        attestation.lock.signable ++
        attestation.known.signable ++
        attestation.responses.signable

    implicit val commitment64AttestationSignable: ContainsSignable[Attestation.Commitment64] =
      attestation =>
        attestation.lock.signable ++
        attestation.known.signable ++
        attestation.responses.signable

    // responses is not used when creating the signable bytes
    implicit val attestationSignable: ContainsSignable[Attestation] = _.value match {
      case Attestation.Value.Predicate(a)    => predicateAttestationSignable.signableBytes(a)
      case Attestation.Value.Image32(a)      => image32AttestationSignable.signableBytes(a)
      case Attestation.Value.Image64(a)      => image64AttestationSignable.signableBytes(a)
      case Attestation.Value.Commitment32(a) => commitment32AttestationSignable.signableBytes(a)
      case Attestation.Value.Commitment64(a) => commitment64AttestationSignable.signableBytes(a)
    }

    implicit val eonEventSignable: ContainsSignable[Event.Eon] =
      event =>
        event.beginSlot.signable ++
        event.height.signable

    implicit val eraEventSignable: ContainsSignable[Event.Era] =
      event =>
        event.beginSlot.signable ++
        event.height.signable

    implicit val epochEventSignable: ContainsSignable[Event.Epoch] =
      event =>
        event.beginSlot.signable ++
        event.height.signable

    implicit val headerEventSignable: ContainsSignable[Event.Header] =
      event => event.height.signable

    implicit val rootEventSignable: ContainsSignable[Event.Root] =
      event => event.value.signable

    implicit val iotxEventSignable: ContainsSignable[Event.IoTransaction] =
      event =>
        event.schedule.signable ++
        event.references32.signable ++
        event.metadata.signable

    implicit val stxoEventSignable: ContainsSignable[Event.SpentTransactionOutput] =
      event => event.metadata.signable

    implicit val utxoEventSignable: ContainsSignable[Event.UnspentTransactionOutput] =
      event => event.metadata.signable

    implicit val eventSignable: ContainsSignable[Event] = _.value match {
      case Event.Value.Eon(e)                      => eonEventSignable.signableBytes(e)
      case Event.Value.Era(e)                      => eraEventSignable.signableBytes(e)
      case Event.Value.Epoch(e)                    => epochEventSignable.signableBytes(e)
      case Event.Value.Header(e)                   => headerEventSignable.signableBytes(e)
      case Event.Value.Root(e)                     => rootEventSignable.signableBytes(e)
      case Event.Value.IoTransaction(e)            => iotxEventSignable.signableBytes(e)
      case Event.Value.SpentTransactionOutput(e)   => stxoEventSignable.signableBytes(e)
      case Event.Value.UnspentTransactionOutput(e) => utxoEventSignable.signableBytes(e)
    }

    implicit val proofSignable: ContainsSignable[Proof] = _ => Array.emptyByteArray

    implicit val lockedSignable: ContainsSignable[Proposition.Locked] =
      _ => Tokens.Locked.signable

    implicit val digestPropositionSignable: ContainsSignable[Proposition.Digest] =
      p =>
        Tokens.Digest.signable ++
        p.routine.signable ++
        p.digest.signable

    implicit val signatureSignable: ContainsSignable[Proposition.DigitalSignature] =
      p =>
        Tokens.DigitalSignature.signable ++
        p.routine.signable ++
        p.verificationKey.signable

    implicit val heightRangeSignable: ContainsSignable[Proposition.HeightRange] =
      p =>
        Tokens.HeightRange.signable ++
        p.chain.signable ++
        p.min.signable ++
        p.max.signable

    implicit val tickRangeSignable: ContainsSignable[Proposition.TickRange] =
      p =>
        Tokens.TickRange.signable ++
        p.min.signable ++
        p.max.signable

    implicit val exactMatchSignable: ContainsSignable[Proposition.ExactMatch] =
      p =>
        Tokens.ExactMatch.signable ++
        p.location.signable ++
        p.compareTo.signable

    implicit val lessThanSignable: ContainsSignable[Proposition.LessThan] =
      p =>
        Tokens.LessThan.signable ++
        p.location.signable ++
        p.compareTo.signable

    implicit val greaterThanSignable: ContainsSignable[Proposition.GreaterThan] =
      p =>
        Tokens.GreaterThan.signable ++
        p.location.signable ++
        p.compareTo.signable

    implicit val equalToSignable: ContainsSignable[Proposition.EqualTo] =
      p =>
        Tokens.EqualTo.signable ++
        p.location.signable ++
        p.compareTo.signable

    implicit val thresholdSignable: ContainsSignable[Proposition.Threshold] =
      p =>
        Tokens.Threshold.signable ++
        p.threshold.signable ++
        p.challenges.signable

    implicit val notSignable: ContainsSignable[Proposition.Not] = p =>
      Tokens.Not.signable ++
      p.proposition.signable

    implicit val andSignable: ContainsSignable[Proposition.And] = p =>
      Tokens.And.signable ++
      p.left.signable ++
      p.right.signable

    implicit val orSignable: ContainsSignable[Proposition.Or] = p =>
      Tokens.Or.signable ++
      p.left.signable ++
      p.right.signable

    implicit val propositionSignable: ContainsSignable[Proposition] = _.value match {
      case Proposition.Value.Locked(p)           => lockedSignable.signableBytes(p)
      case Proposition.Value.Digest(p)           => digestPropositionSignable.signableBytes(p)
      case Proposition.Value.DigitalSignature(p) => signatureSignable.signableBytes(p)
      case Proposition.Value.HeightRange(p)      => heightRangeSignable.signableBytes(p)
      case Proposition.Value.TickRange(p)        => tickRangeSignable.signableBytes(p)
      case Proposition.Value.ExactMatch(p)       => exactMatchSignable.signableBytes(p)
      case Proposition.Value.LessThan(p)         => lessThanSignable.signableBytes(p)
      case Proposition.Value.GreaterThan(p)      => greaterThanSignable.signableBytes(p)
      case Proposition.Value.EqualTo(p)          => equalToSignable.signableBytes(p)
      case Proposition.Value.Threshold(p)        => thresholdSignable.signableBytes(p)
      case Proposition.Value.Not(p)              => notSignable.signableBytes(p)
      case Proposition.Value.And(p)              => andSignable.signableBytes(p)
      case Proposition.Value.Or(p)               => orSignable.signableBytes(p)
    }

  }
  object instances extends Instances
}
