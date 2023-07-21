package co.topl.brambl.servicekit

import cats.data.ValidatedNel
import cats.effect.kernel.Sync
import co.topl.brambl.builders.locks.LockTemplate
import co.topl.brambl.dataApi.WalletStateAlgebra
import co.topl.brambl.models.Indices
import co.topl.brambl.models.box.Lock
import quivr.models.Preimage
import quivr.models.Proposition
import quivr.models.VerificationKey

object WalletStateApi {

  def make[F[_]: Sync](): WalletStateAlgebra[F] =
    new WalletStateAlgebra[F] {

      override def getIndicesBySignature(signatureProposition: Proposition.DigitalSignature): F[Option[Indices]] = ???

      def getLockByIndex(indices: Indices): F[Option[Lock.Predicate]] = ???

      override def updateWalletState(
        lockPredicate: String,
        lockAddress:   String,
        routine:       Option[String],
        vk:            Option[String],
        indices:       Indices
      ): F[Unit] = ???

      override def getNextIndicesForFunds(party: String, contract: String): F[Option[Indices]] = ???

      def validateCurrentIndicesForFunds(
        party:     String,
        contract:  String,
        someState: Option[Int]
      ): F[ValidatedNel[String, Indices]] = ???

      override def getAddress(
        party:     String,
        contract:  String,
        someState: Option[Int]
      ): F[Option[String]] = ???

      override def getCurrentIndicesForFunds(
        party:     String,
        contract:  String,
        someState: Option[Int]
      ): F[Option[Indices]] = ???

      override def getCurrentAddress: F[String] = ???

      override def initWalletState(
        vk: VerificationKey
      ): F[Unit] = ???

      override def getPreimage(
        digestProposition: Proposition.Digest
      ): F[Option[Preimage]] = ???

      override def addEntityVks(
        party:    String,
        contract: String,
        entities: List[String]
      ): F[Unit] = ???

      override def getEntityVks(
        party:    String,
        contract: String
      ): F[Option[List[String]]] = ???

      override def addNewLockTemplate(
        contract:     String,
        lockTemplate: LockTemplate[F]
      ): F[Unit] = ???

      override def getLockTemplate(
        contract: String
      ): F[Option[LockTemplate[F]]] = ???

      override def getLock(
        party:     String,
        contract:  String,
        nextState: Int
      ): F[Option[Lock]] = ???
    }
}
