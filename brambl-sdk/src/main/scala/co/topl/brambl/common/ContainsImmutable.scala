package co.topl.brambl.common

import co.topl.brambl.models._
import co.topl.brambl.models.box.{Box, Lock, Value}
import co.topl.brambl.models.common.ImmutableBytes
import co.topl.brambl.models.transaction._
import co.topl.quivr.Tokens
import com.google.protobuf.ByteString
import quivr.models._

import java.nio.charset.StandardCharsets
import scala.language.implicitConversions

trait ContainsImmutable[T] {
  def immutableBytes(t: T): ImmutableBytes
}

object ContainsImmutable {

  def apply[T](implicit ev: ContainsImmutable[T]): ContainsImmutable[T] = ev

  implicit class ContainsImmutableTOps[T: ContainsImmutable](t: T) {
    def immutable: ImmutableBytes = ContainsImmutable[T].immutableBytes(t)
  }

  implicit class ImmutableOps(val t: ImmutableBytes) extends AnyVal {
    def ++(other: ImmutableBytes): ImmutableBytes = ImmutableBytes(t.value.concat(other.value))
  }

  trait Instances {

    implicit private def arrayByteToImmutableBytes(bytes: Array[Byte]): ImmutableBytes =
      ImmutableBytes(ByteString.copyFrom(bytes))

    implicit val byteImmutable: ContainsImmutable[Byte] = (t: Byte) => Array(t)

    implicit val arrayByteImmutable: ContainsImmutable[Array[Byte]] = (bytes: Array[Byte]) => bytes

    implicit val byteStringImmutable: ContainsImmutable[ByteString] = _.toByteArray.immutable

    implicit val intImmutable: ContainsImmutable[Int] = (int: Int) => BigInt(int).toByteArray

    implicit val longImmutable: ContainsImmutable[Long] = (long: Long) => BigInt(long).toByteArray

    implicit val stringImmutable: ContainsImmutable[String] = (string: String) =>
      string.getBytes(StandardCharsets.UTF_8)

    implicit def seqImmutable[T: ContainsImmutable]: ContainsImmutable[Seq[T]] = (seq: Seq[T]) =>
      seq.zipWithIndex.foldLeft(ImmutableBytes()) { case (acc, (item, index)) =>
        acc ++
        index.immutable ++
        item.immutable
      }

    implicit def optionImmutable[T: ContainsImmutable]: ContainsImmutable[Option[T]] = (option: Option[T]) =>
      option.fold(Array(0xff.toByte): ImmutableBytes)(_.immutable)

    implicit val int128Immutable: ContainsImmutable[Int128] = _.value.immutable

    implicit val smallDataImmutable: ContainsImmutable[SmallData] = _.value.immutable

    implicit val rootImmutable: ContainsImmutable[Root] = _.value match {
      case Root.Value.Root32(v) => v.immutable
      case Root.Value.Root64(v) => v.immutable
    }

    implicit val verificationKeyImmutable: ContainsImmutable[VerificationKey] = _.value.immutable

    implicit val witnessImmutable: ContainsImmutable[Witness] = _.value.immutable

    implicit val datumImmutable: ContainsImmutable[Datum] = _.value match {
      case Datum.Value.Eon(v)           => v.immutable
      case Datum.Value.Era(v)           => v.immutable
      case Datum.Value.Epoch(v)         => v.immutable
      case Datum.Value.Header(v)        => v.immutable
      case Datum.Value.Root(v)          => v.immutable
      case Datum.Value.IoTransaction(v) => v.immutable
      case Datum.Value.SpentOutput(v)   => v.immutable
      case Datum.Value.UnspentOutput(v) => v.immutable
    }

    implicit val eonDatumImmutable: ContainsImmutable[Datum.Eon] = _.event.immutable
    implicit val eraDatumImmutable: ContainsImmutable[Datum.Era] = _.event.immutable
    implicit val epochDatumImmutable: ContainsImmutable[Datum.Epoch] = _.event.immutable
    implicit val headerDatumImmutable: ContainsImmutable[Datum.Header] = _.event.immutable
    implicit val rootDatumImmutable: ContainsImmutable[Datum.Root] = _.event.immutable
    implicit val ioTransactionDatumImmutable: ContainsImmutable[Datum.IoTransaction] = _.event.immutable
    implicit val spentOutputDatumImmutable: ContainsImmutable[Datum.SpentOutput] = _.event.immutable
    implicit val unspentOutputDatumImmutable: ContainsImmutable[Datum.UnspentOutput] = _.event.immutable

    implicit val ioTransactionImmutable: ContainsImmutable[IoTransaction] = (iotx: IoTransaction) =>
      iotx.inputs.immutable ++
      iotx.outputs.immutable ++
      iotx.datum.immutable

    implicit val iotxScheduleImmutable: ContainsImmutable[Schedule] = (schedule: Schedule) =>
      schedule.min.immutable ++
      schedule.max.immutable

    implicit val spentOutputImmutable: ContainsImmutable[SpentTransactionOutput] = (stxo: SpentTransactionOutput) =>
      stxo.knownIdentifier.immutable ++
      stxo.attestation.immutable ++
      stxo.value.immutable ++
      stxo.datum.immutable

    implicit val unspentOutputImmutable: ContainsImmutable[UnspentTransactionOutput] =
      (utxo: UnspentTransactionOutput) =>
        utxo.address.immutable ++
        utxo.value.immutable ++
        utxo.datum.immutable

    implicit val boxImmutable: ContainsImmutable[Box] = (box: Box) =>
      box.lock.immutable ++
      box.value.immutable

    implicit val valueImmutable: ContainsImmutable[Value] = _.value match {
      case Value.Value.Token(v) => v.immutable
      case Value.Value.Asset(v) => v.immutable
    }

    implicit val addressImmutable: ContainsImmutable[Address] = (address: Address) =>
      address.network.immutable ++
      address.ledger.immutable ++
      address.identifier.immutable

    implicit val size32EvidenceImmutable: ContainsImmutable[Evidence.Sized32] =
      ev => ev.digest.immutable

    implicit val size64EvidenceImmutable: ContainsImmutable[Evidence.Sized64] =
      ev => ev.digest.immutable

    implicit val evidenceImmutable: ContainsImmutable[Evidence] = _.value match {
      case Evidence.Value.Sized32(e) => size32EvidenceImmutable.immutableBytes(e)
      case Evidence.Value.Sized64(e) => size64EvidenceImmutable.immutableBytes(e)
    }

    implicit val digest32Immutable: ContainsImmutable[Digest.Digest32] = _.value.immutable
    implicit val digest64Immutable: ContainsImmutable[Digest.Digest64] = _.value.immutable

    implicit val digestImmutable: ContainsImmutable[Digest] = _.value match {
      case Digest.Value.Digest32(v) => v.immutable
      case Digest.Value.Digest64(v) => v.immutable
    }

    implicit val preimageImmutable: ContainsImmutable[Preimage] = (pre: Preimage) =>
      pre.input.immutable ++ pre.salt.immutable

    implicit val accumulatorRoot32IdentifierImmutable: ContainsImmutable[Identifier.AccumulatorRoot32] =
      id =>
        Tags.Identifier.AccumulatorRoot32.immutable ++
        id.evidence.immutable

    implicit val accumulatorRoot64IdentifierImmutable: ContainsImmutable[Identifier.AccumulatorRoot64] =
      id =>
        Tags.Identifier.AccumulatorRoot64.immutable ++
        id.evidence.immutable

    implicit val boxLock32IdentifierImmutable: ContainsImmutable[Identifier.Lock32] =
      id =>
        Tags.Identifier.Lock32.immutable ++
        id.evidence.immutable

    implicit val boxLock64IdentifierImmutable: ContainsImmutable[Identifier.Lock64] =
      id =>
        Tags.Identifier.Lock64.immutable ++
        id.evidence.immutable

    implicit val boxValue32IdentifierImmutable: ContainsImmutable[Identifier.BoxValue32] =
      id =>
        Tags.Identifier.BoxValue32.immutable ++
        id.evidence.immutable

    implicit val boxValue64IdentifierImmutable: ContainsImmutable[Identifier.BoxValue64] =
      id =>
        Tags.Identifier.BoxValue64.immutable ++
        id.evidence.immutable

    implicit val ioTransaction32IdentifierImmutable: ContainsImmutable[Identifier.IoTransaction32] =
      id =>
        Tags.Identifier.IoTransaction32.immutable ++
        id.evidence.immutable

    implicit val ioTransaction64IdentifierImmutable: ContainsImmutable[Identifier.IoTransaction64] =
      id =>
        Tags.Identifier.IoTransaction64.immutable ++
        id.evidence.immutable

    implicit val identifiersImmutable: ContainsImmutable[Identifier] = _.value match {
      case Identifier.Value.AccumulatorRoot32(i) => accumulatorRoot32IdentifierImmutable.immutableBytes(i)
      case Identifier.Value.AccumulatorRoot64(i) => accumulatorRoot64IdentifierImmutable.immutableBytes(i)
      case Identifier.Value.Lock32(i)            => boxLock32IdentifierImmutable.immutableBytes(i)
      case Identifier.Value.Lock64(i)            => boxLock64IdentifierImmutable.immutableBytes(i)
      case Identifier.Value.BoxValue32(i)        => boxValue32IdentifierImmutable.immutableBytes(i)
      case Identifier.Value.BoxValue64(i)        => boxValue64IdentifierImmutable.immutableBytes(i)
      case Identifier.Value.IoTransaction32(i)   => ioTransaction32IdentifierImmutable.immutableBytes(i)
      case Identifier.Value.IoTransaction64(i)   => ioTransaction64IdentifierImmutable.immutableBytes(i)
    }

    implicit val knownOutput32IdentifierImmutable: ContainsImmutable[KnownIdentifier.TransactionOutput32] =
      (knownId: KnownIdentifier.TransactionOutput32) =>
        knownId.network.immutable ++
        knownId.ledger.immutable ++
        knownId.index.immutable ++
        knownId.id.immutable

    implicit val knownOutput64IdentifierImmutable: ContainsImmutable[KnownIdentifier.TransactionOutput64] =
      (knownId: KnownIdentifier.TransactionOutput64) =>
        knownId.network.immutable ++
        knownId.ledger.immutable ++
        knownId.index.immutable ++
        knownId.id.immutable

    implicit val knownIdentifierImmutable: ContainsImmutable[KnownIdentifier] = _.value match {
      case KnownIdentifier.Value.TransactionOutput32(r) => knownOutput32IdentifierImmutable.immutableBytes(r)
      case KnownIdentifier.Value.TransactionOutput64(r) => knownOutput64IdentifierImmutable.immutableBytes(r)
    }

    implicit val tokenValueImmutable: ContainsImmutable[Value.Token] = (token: Value.Token) => token.quantity.immutable

    implicit val assetValueImmutable: ContainsImmutable[Value.Asset] = (asset: Value.Asset) =>
      asset.label.immutable ++
      asset.quantity.immutable ++
      asset.metadata.immutable

    // consider making predicate non-empty
    implicit val predicateLockImmutable: ContainsImmutable[Lock.Predicate] = (predicate: Lock.Predicate) =>
      predicate.threshold.immutable ++
      predicate.challenges.immutable

    implicit val image32LockImmutable: ContainsImmutable[Lock.Image32] = (image: Lock.Image32) =>
      image.threshold.immutable ++
      image.leaves.immutable

    implicit val image64LockImmutable: ContainsImmutable[Lock.Image64] = (image: Lock.Image64) =>
      image.threshold.immutable ++
      image.leaves.immutable

    implicit val commitment32LockImmutable: ContainsImmutable[Lock.Commitment32] = (commitment: Lock.Commitment32) =>
      commitment.threshold.immutable ++
      commitment.root.size.immutable ++
      commitment.root.immutable

    implicit val commitment64LockImmutable: ContainsImmutable[Lock.Commitment64] = (commitment: Lock.Commitment64) =>
      commitment.threshold.immutable ++
      commitment.root.size.immutable ++
      commitment.root.immutable

    implicit val lockImmutable: ContainsImmutable[Lock] = _.value match {
      case Lock.Value.Predicate(l)    => predicateLockImmutable.immutableBytes(l)
      case Lock.Value.Image32(l)      => image32LockImmutable.immutableBytes(l)
      case Lock.Value.Image64(l)      => image64LockImmutable.immutableBytes(l)
      case Lock.Value.Commitment32(l) => commitment32LockImmutable.immutableBytes(l)
      case Lock.Value.Commitment64(l) => commitment64LockImmutable.immutableBytes(l)
    }

    implicit val predicateAttestationImmutable: ContainsImmutable[Attestation.Predicate] =
      attestation =>
        attestation.lock.immutable ++
        attestation.responses.immutable

    implicit val image32AttestationImmutable: ContainsImmutable[Attestation.Image32] =
      attestation =>
        attestation.lock.immutable ++
        attestation.known.immutable ++
        attestation.responses.immutable

    implicit val image64AttestationImmutable: ContainsImmutable[Attestation.Image64] =
      attestation =>
        attestation.lock.immutable ++
        attestation.known.immutable ++
        attestation.responses.immutable

    implicit val commitment32AttestationImmutable: ContainsImmutable[Attestation.Commitment32] =
      attestation =>
        attestation.lock.immutable ++
        attestation.known.immutable ++
        attestation.responses.immutable

    implicit val commitment64AttestationImmutable: ContainsImmutable[Attestation.Commitment64] =
      attestation =>
        attestation.lock.immutable ++
        attestation.known.immutable ++
        attestation.responses.immutable

    implicit val attestationImmutable: ContainsImmutable[Attestation] = _.value match {
      case Attestation.Value.Predicate(a)    => predicateAttestationImmutable.immutableBytes(a)
      case Attestation.Value.Image32(a)      => image32AttestationImmutable.immutableBytes(a)
      case Attestation.Value.Image64(a)      => image64AttestationImmutable.immutableBytes(a)
      case Attestation.Value.Commitment32(a) => commitment32AttestationImmutable.immutableBytes(a)
      case Attestation.Value.Commitment64(a) => commitment64AttestationImmutable.immutableBytes(a)
    }

    implicit val eonEventImmutable: ContainsImmutable[Event.Eon] =
      event =>
        event.beginSlot.immutable ++
        event.height.immutable

    implicit val eraEventImmutable: ContainsImmutable[Event.Era] =
      event =>
        event.beginSlot.immutable ++
        event.height.immutable

    implicit val epochEventImmutable: ContainsImmutable[Event.Epoch] =
      event =>
        event.beginSlot.immutable ++
        event.height.immutable

    implicit val headerEventImmutable: ContainsImmutable[Event.Header] =
      event => event.height.immutable

    implicit val rootEventImmutable: ContainsImmutable[Event.Root] =
      event => event.value.immutable

    implicit val iotxEventImmutable: ContainsImmutable[Event.IoTransaction] =
      event =>
        event.schedule.immutable ++
        event.references32.immutable ++
        event.metadata.immutable

    implicit val stxoEventImmutable: ContainsImmutable[Event.SpentTransactionOutput] =
      event => event.metadata.immutable

    implicit val utxoEventImmutable: ContainsImmutable[Event.UnspentTransactionOutput] =
      event => event.metadata.immutable

    implicit val eventImmutable: ContainsImmutable[Event] = _.value match {
      case Event.Value.Eon(e)                      => eonEventImmutable.immutableBytes(e)
      case Event.Value.Era(e)                      => eraEventImmutable.immutableBytes(e)
      case Event.Value.Epoch(e)                    => epochEventImmutable.immutableBytes(e)
      case Event.Value.Header(e)                   => headerEventImmutable.immutableBytes(e)
      case Event.Value.Root(e)                     => rootEventImmutable.immutableBytes(e)
      case Event.Value.IoTransaction(e)            => iotxEventImmutable.immutableBytes(e)
      case Event.Value.SpentTransactionOutput(e)   => stxoEventImmutable.immutableBytes(e)
      case Event.Value.UnspentTransactionOutput(e) => utxoEventImmutable.immutableBytes(e)
    }

    implicit val txBindImmutable: ContainsImmutable[TxBind] = _.value.immutable

    implicit val lockedImmutable: ContainsImmutable[Proposition.Locked] =
      _ => Tokens.Locked.immutable

    implicit val lockedProofImmutable: ContainsImmutable[Proof.Locked] =
      _ => Array.emptyByteArray

    implicit val digestPropositionImmutable: ContainsImmutable[Proposition.Digest] =
      p =>
        Tokens.Digest.immutable ++
        p.routine.immutable ++
        p.digest.immutable

    implicit val digestProofImmutable: ContainsImmutable[Proof.Digest] =
      p =>
        p.transactionBind.immutable ++
        p.preimage.immutable

    implicit val signatureImmutable: ContainsImmutable[Proposition.DigitalSignature] =
      p =>
        Tokens.DigitalSignature.immutable ++
        p.routine.immutable ++
        p.verificationKey.immutable

    implicit val signatureProofImmutable: ContainsImmutable[Proof.DigitalSignature] =
      p =>
        p.transactionBind.immutable ++
        p.witness.immutable

    implicit val heightRangeImmutable: ContainsImmutable[Proposition.HeightRange] =
      p =>
        Tokens.HeightRange.immutable ++
        p.chain.immutable ++
        p.min.immutable ++
        p.max.immutable

    implicit val heightRangeProofImmutable: ContainsImmutable[Proof.HeightRange] = _.transactionBind.immutable

    implicit val tickRangeImmutable: ContainsImmutable[Proposition.TickRange] =
      p =>
        Tokens.TickRange.immutable ++
        p.min.immutable ++
        p.max.immutable

    implicit val tickRangeProofImmutable: ContainsImmutable[Proof.TickRange] = _.transactionBind.immutable

    implicit val exactMatchImmutable: ContainsImmutable[Proposition.ExactMatch] =
      p =>
        Tokens.ExactMatch.immutable ++
        p.location.immutable ++
        p.compareTo.immutable

    implicit val exactMatchProofImmutable: ContainsImmutable[Proof.ExactMatch] = _.transactionBind.immutable

    implicit val lessThanImmutable: ContainsImmutable[Proposition.LessThan] =
      p =>
        Tokens.LessThan.immutable ++
        p.location.immutable ++
        p.compareTo.immutable

    implicit val lessThanProofImmutable: ContainsImmutable[Proof.LessThan] = _.transactionBind.immutable

    implicit val greaterThanImmutable: ContainsImmutable[Proposition.GreaterThan] =
      p =>
        Tokens.GreaterThan.immutable ++
        p.location.immutable ++
        p.compareTo.immutable

    implicit val greaterThanProofImmutable: ContainsImmutable[Proof.GreaterThan] = _.transactionBind.immutable

    implicit val equalToImmutable: ContainsImmutable[Proposition.EqualTo] =
      p =>
        Tokens.EqualTo.immutable ++
        p.location.immutable ++
        p.compareTo.immutable

    implicit val equalToProofImmutable: ContainsImmutable[Proof.EqualTo] = _.transactionBind.immutable

    implicit val thresholdImmutable: ContainsImmutable[Proposition.Threshold] =
      p =>
        Tokens.Threshold.immutable ++
        p.threshold.immutable ++
        p.challenges.immutable

    implicit val thresholdProofImmutable: ContainsImmutable[Proof.Threshold] =
      p =>
        p.transactionBind.immutable ++
        p.responses.immutable

    implicit val notImmutable: ContainsImmutable[Proposition.Not] = p =>
      Tokens.Not.immutable ++
      p.proposition.immutable

    implicit val notProofImmutable: ContainsImmutable[Proof.Not] = p => p.transactionBind.immutable ++ p.proof.immutable

    implicit val andImmutable: ContainsImmutable[Proposition.And] = p =>
      Tokens.And.immutable ++
      p.left.immutable ++
      p.right.immutable

    implicit val andProofImmutable: ContainsImmutable[Proof.And] = p =>
      p.transactionBind.immutable ++
      p.left.immutable ++
      p.right.immutable

    implicit val orImmutable: ContainsImmutable[Proposition.Or] = p =>
      Tokens.Or.immutable ++
      p.left.immutable ++
      p.right.immutable

    implicit val orProofImmutable: ContainsImmutable[Proof.Or] = p =>
      p.transactionBind.immutable ++
      p.left.immutable ++
      p.right.immutable

    implicit val propositionImmutable: ContainsImmutable[Proposition] = _.value match {
      case Proposition.Value.Locked(p)           => lockedImmutable.immutableBytes(p)
      case Proposition.Value.Digest(p)           => digestPropositionImmutable.immutableBytes(p)
      case Proposition.Value.DigitalSignature(p) => signatureImmutable.immutableBytes(p)
      case Proposition.Value.HeightRange(p)      => heightRangeImmutable.immutableBytes(p)
      case Proposition.Value.TickRange(p)        => tickRangeImmutable.immutableBytes(p)
      case Proposition.Value.ExactMatch(p)       => exactMatchImmutable.immutableBytes(p)
      case Proposition.Value.LessThan(p)         => lessThanImmutable.immutableBytes(p)
      case Proposition.Value.GreaterThan(p)      => greaterThanImmutable.immutableBytes(p)
      case Proposition.Value.EqualTo(p)          => equalToImmutable.immutableBytes(p)
      case Proposition.Value.Threshold(p)        => thresholdImmutable.immutableBytes(p)
      case Proposition.Value.Not(p)              => notImmutable.immutableBytes(p)
      case Proposition.Value.And(p)              => andImmutable.immutableBytes(p)
      case Proposition.Value.Or(p)               => orImmutable.immutableBytes(p)
    }

    implicit val proofImmutable: ContainsImmutable[Proof] = _.value match {
      case Proof.Value.Locked(p)           => lockedProofImmutable.immutableBytes(p)
      case Proof.Value.Digest(p)           => digestProofImmutable.immutableBytes(p)
      case Proof.Value.DigitalSignature(p) => signatureProofImmutable.immutableBytes(p)
      case Proof.Value.HeightRange(p)      => heightRangeProofImmutable.immutableBytes(p)
      case Proof.Value.TickRange(p)        => tickRangeProofImmutable.immutableBytes(p)
      case Proof.Value.ExactMatch(p)       => exactMatchProofImmutable.immutableBytes(p)
      case Proof.Value.LessThan(p)         => lessThanProofImmutable.immutableBytes(p)
      case Proof.Value.GreaterThan(p)      => greaterThanProofImmutable.immutableBytes(p)
      case Proof.Value.EqualTo(p)          => equalToProofImmutable.immutableBytes(p)
      case Proof.Value.Threshold(p)        => thresholdProofImmutable.immutableBytes(p)
      case Proof.Value.Not(p)              => notProofImmutable.immutableBytes(p)
      case Proof.Value.And(p)              => andProofImmutable.immutableBytes(p)
      case Proof.Value.Or(p)               => orProofImmutable.immutableBytes(p)
      case Proof.Value.Empty               => Array.emptyByteArray
    }
  }
  object instances extends Instances
}
