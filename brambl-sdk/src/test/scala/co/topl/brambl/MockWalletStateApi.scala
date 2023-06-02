package co.topl.brambl

import cats.Id
import cats.data.ValidatedNel
import co.topl.brambl.builders.locks.LockTemplate
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.models._
import co.topl.brambl.models.box.Lock
import co.topl.brambl.wallet.WalletStateAlgebra
import quivr.models._

/**
 * Mock Implementation of the WalletStateAlgebra for testing
 */
object MockWalletStateApi extends WalletStateAlgebra[Id] with MockHelpers {

  val propEvidenceToIdx: Map[Evidence, Indices] = Map(
    MockSignatureProposition.value.digitalSignature.get.sizedEvidence -> MockIndices
  )

  val propEvidenceToPreimage: Map[Evidence, Preimage] = Map(
    MockDigestProposition.value.digest.get.sizedEvidence -> MockPreimage
  )

  override def getIndicesBySignature(signatureProposition: Proposition.DigitalSignature): Option[Indices] =
    propEvidenceToIdx.get(signatureProposition.sizedEvidence)

  override def getPreimage(digestProposition: Proposition.Digest): Option[Preimage] =
    propEvidenceToPreimage.get(digestProposition.sizedEvidence)

  // The following are not implemented since they are not used in the tests
  override def initWalletState(vk: VerificationKey): Id[Unit] = ???

  override def getCurrentAddress: Id[String] = ???

  override def updateWalletState(
    lockPredicate: String,
    lockAddress:   String,
    routine:       Option[String],
    vk:            Option[String],
    indices:       Indices
  ): Id[Unit] = ???

  override def getCurrentIndicesForFunds(party: String, contract: String, someState: Option[Int]): Id[Option[Indices]] =
    ???

  override def validateCurrentIndicesForFunds(
    party:     String,
    contract:  String,
    someState: Option[Int]
  ): Id[ValidatedNel[String, Indices]] = ???

  override def getNextIndicesForFunds(party: String, contract: String): Id[Option[Indices]] = ???

  override def getLockByIndex(indices: Indices): Id[Option[Lock.Predicate]] = ???

  override def getAddress(party: String, contract: String, someState: Option[Int]): Id[Option[String]] = ???

  override def addEntityVks(party: String, contract: String, entities: List[String]): Id[Unit] = ???

  override def getEntityVks(party: String, contract: String): Id[Option[List[String]]] = ???

  override def addNewLockTemplate(contract: String, lockTemplate: LockTemplate[Id]): Id[Unit] = ???

  override def getLockTemplate(contract: String): Id[Option[LockTemplate[Id]]] = ???

  override def getLock(party: String, contract: String, nextState: Int): Id[Option[Lock]] = ???
}
