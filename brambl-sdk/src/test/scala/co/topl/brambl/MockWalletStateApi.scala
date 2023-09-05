package co.topl.brambl

import cats.Id
import cats.data.ValidatedNel
import cats.effect.IO
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.builders.locks.LockTemplate
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.dataApi.WalletStateAlgebra
import co.topl.brambl.models._
import co.topl.brambl.models.box.Lock
import quivr.models._

/**
 * Mock Implementation of the WalletStateAlgebra for testing
 */
object MockWalletStateApi extends WalletStateAlgebra[IO] with MockHelpers {

  val propEvidenceToIdx: Map[Evidence, Indices] = Map(
    MockSignatureProposition.value.digitalSignature.get.sizedEvidence -> MockIndices
  )

  val propEvidenceToPreimage: Map[Evidence, Preimage] = Map(
    MockDigestProposition.value.digest.get.sizedEvidence -> MockPreimage
  )

  val lockAddressToLock: Map[String, Lock.Predicate] = Map(
    trivialLockAddress.toBase58() -> trivialOutLock.getPredicate
  )

  override def getIndicesBySignature(signatureProposition: Proposition.DigitalSignature): F[Option[Indices]] =
    IO.pure(propEvidenceToIdx.get(signatureProposition.sizedEvidence))

  override def getPreimage(digestProposition: Proposition.Digest): F[Option[Preimage]] =
    IO.pure(propEvidenceToPreimage.get(digestProposition.sizedEvidence))

  override def getLockByAddress(lockAddress: String): F[Option[Lock.Predicate]] =
    IO.pure(lockAddressToLock.get(lockAddress))

  // The following are not implemented since they are not used in the tests
  override def initWalletState(networkId: Int, ledgerId: Int, vk: VerificationKey): F[Unit] = ???

  override def getCurrentAddress: F[String] = ???

  override def updateWalletState(
    lockPredicate: String,
    lockAddress:   String,
    routine:       Option[String],
    vk:            Option[String],
    indices:       Indices
  ): F[Unit] = ???

  override def getCurrentIndicesForFunds(party: String, contract: String, someState: Option[Int]): F[Option[Indices]] =
    ???

  override def validateCurrentIndicesForFunds(
    party:     String,
    contract:  String,
    someState: Option[Int]
  ): F[ValidatedNel[String, Indices]] = ???

  override def getNextIndicesForFunds(party: String, contract: String): F[Option[Indices]] = ???

  override def getLockByIndex(indices: Indices): F[Option[Lock.Predicate]] = ???

  override def getAddress(party: String, contract: String, someState: Option[Int]): F[Option[String]] = ???

  override def addEntityVks(party: String, contract: String, entities: List[String]): F[Unit] = ???

  override def getEntityVks(party: String, contract: String): F[Option[List[String]]] = ???

  override def addNewLockTemplate(contract: String, lockTemplate: LockTemplate[F]): F[Unit] = ???

  override def getLockTemplate(contract: String): F[Option[LockTemplate[F]]] = ???

  override def getLock(party: String, contract: String, nextState: Int): F[Option[Lock]] = ???
}
