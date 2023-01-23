package co.topl.brambl

import cats.Id
import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.routines.digests.validators.Blake2b256DigestInterpreter
import co.topl.brambl.typeclasses.ContainsSignable.instances.ioTransactionSignable
import co.topl.brambl.validation.Ed25519SignatureInterpreter
import co.topl.common.ParsableDataInterface
import co.topl.quivr.runtime.DynamicContext
import co.topl.quivr.algebras.{DigestVerifier, SignatureVerifier}
import quivr.models.SignableBytes

// A Verification Context opinionated to the Topl context.
// signableBytes, currentTick and the datums are dynamic
case class Context(tx: IoTransaction, curTick: Long, heightDatums: String => Option[Datum])
    extends DynamicContext[Id, String, Datum] {

  override val hashingRoutines: Map[String, DigestVerifier[Id]] =
    Map("blake2b256" -> Blake2b256DigestInterpreter)

  override val signingRoutines: Map[String, SignatureVerifier[Id]] =
    Map("ed25519" -> Ed25519SignatureInterpreter.make())

  override val interfaces: Map[String, ParsableDataInterface] = Map() // Arbitrary

  override def signableBytes: Id[SignableBytes] = ioTransactionSignable.signableBytes(tx)

  override def currentTick: Id[Long] = curTick

  // Needed for height
  override val datums: String => Option[Datum] = heightDatums

  def heightOf(label: String): Id[Option[Long]] = heightDatums(label).flatMap(_.value match {
    case Datum.Value.Header(h) => h.event.map(_.height)
    case _                     => None
  })
}
