package co.topl.brambl.builders

import cats.Monad
import cats.data.EitherT
import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.models.{Datum, Event, GroupId, LockAddress, LockId, TransactionOutputAddress}
import co.topl.brambl.models.box.{Attestation, Lock, Value}
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.genus.services.Txo
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof, SmallData}
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._
import cats.implicits._
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.dataApi.{BifrostQueryAlgebra, WalletStateAlgebra}
import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.syntax.groupPolicyAsGroupPolicySyntaxOps

/**
 * Defines a builder for [[IoTransaction]]s
 */
trait TransactionBuilderApi[F[_]] {

  /**
   * Builds an unproven attestation for the given predicate
   *
   * @param lockPredicate The predicate to use to build the unproven attestation
   * @return An unproven attestation
   */
  def unprovenAttestation(lockPredicate: Lock.Predicate): F[Attestation]

  /**
   * Builds a lock address for the given lock
   *
   * @param lock The lock to use to build the lock address
   * @return A lock address
   */
  def lockAddress(lock: Lock): F[LockAddress]

  /**
   * Builds a lvl unspent transaction output for the given predicate lock and amount
   *
   * @param predicate The predicate to use to build the lvl output
   * @param amount The amount to use to build the lvl output
   * @return An unspent transaction output containing lvls
   */
  def lvlOutput(predicate: Lock.Predicate, amount: Int128): F[UnspentTransactionOutput]

  /**
   * Builds a lvl unspent transaction output for the given lock address and amount
   *
   * @param lockAddress The lock address to use to build the lvl output
   * @param amount The amount to use to build the lvl output
   * @return An unspent transaction output containing lvls
   */
  def lvlOutput(lockAddress: LockAddress, amount: Int128): F[UnspentTransactionOutput]

  /**
   * Builds a datum with default values for a transaction. The schedule is defaulted to use the current timestamp, with
   * min and max slot being 0 and Long.MaxValue respectively.
   *
   * @return A transaction datum
   */
  def datum(): F[Datum.IoTransaction]

  /**
   * Builds a simple lvl transaction with the given parameters
   *
   * @param lvlTxos The lvl transaction outputs that are able to be spent in the transaction
   * @param lockPredicateFrom The predicate to use to build the transaction input
   * @param lockPredicateForChange The predicate to use to build the transaction change output
   * @param recipientLockAddress The lock address to use to build the transaction recipient output
   * @param amount The amount to use to build the transaction recipient output
   * @return A simple lvl transaction
   */
  def buildSimpleLvlTransaction(
    lvlTxos:                Seq[Txo],
    lockPredicateFrom:      Lock.Predicate,
    lockPredicateForChange: Lock.Predicate,
    recipientLockAddress:   LockAddress,
    amount:                 Long
  ): F[IoTransaction]

  /**
   * Builds a simple transaction to mint Group Constructor tokens
   *
   * @param walletStateApi API for retrieving wallet state
   * @param bifrostRpc API for interacting with a blockchain node
   * @param groupPolicy The group policy for which we are minting constructor tokens.
   *                    This group policy specifies a registrationUtxo to be used as an input in this transaction. If
   *                    retrieving the data of this UTXO fails (unable to fetch UTXO from the node or unable to retrieve
   *                    its Lock from the wallet), an error will be returned.
   * @param lock A Lock to encumber the newly minted constructor tokens
   * @param quantity The quantity of constructor tokens to mint
   * @return An unproven Group Constructor minting transaction if possible. Else, an error
   */
  def buildSimpleGroupMintingTransaction(
    walletStateApi: WalletStateAlgebra[F],
    bifrostRpc:     BifrostQueryAlgebra[F],
    groupPolicy:    GroupPolicy,
    lock:           Lock,
    quantity:       Int128
  ): F[Either[BuilderError, IoTransaction]]
}

object TransactionBuilderApi {

  object implicits {

    case class LockAddressOps(
      lockAddress: LockAddress
    ) {
      def toBase58(): String = AddressCodecs.encodeAddress(lockAddress)
    }

    implicit def lockAddressOps(
      lockAddress: LockAddress
    ): LockAddressOps = LockAddressOps(lockAddress)

  }

  case class BuildStxoError(message: String, cause: Throwable = null) extends BuilderError(message, cause)

  def make[F[_]: Monad](
    networkId: Int,
    ledgerId:  Int
  ): TransactionBuilderApi[F] =
    new TransactionBuilderApi[F] {

      override def buildSimpleLvlTransaction(
        lvlTxos:                Seq[Txo],
        lockPredicateFrom:      Lock.Predicate,
        lockPredicateForChange: Lock.Predicate,
        recipientLockAddress:   LockAddress,
        amount:                 Long
      ): F[IoTransaction] = for {
        unprovenAttestationToProve <- unprovenAttestation(lockPredicateFrom)
        totalValues =
          lvlTxos
            .foldLeft(
              BigInt(0)
            )((acc, x) =>
              acc + x.transactionOutput.value.value.lvl
                .map(y => BigInt(y.quantity.value.toByteArray))
                .getOrElse(BigInt(0))
            )
        datum <- datum()
        lvlOutputForChange <- lvlOutput(
          lockPredicateForChange,
          Int128(
            ByteString.copyFrom(
              BigInt(totalValues.toLong - amount).toByteArray
            )
          )
        )
        lvlOutputForRecipient <- lvlOutput(
          recipientLockAddress,
          Int128(ByteString.copyFrom(BigInt(amount).toByteArray))
        )
        ioTransaction = IoTransaction.defaultInstance
          .withInputs(
            lvlTxos.map(x =>
              SpentTransactionOutput(
                x.outputAddress,
                unprovenAttestationToProve,
                x.transactionOutput.value
              )
            )
          )
          .withOutputs(
            // If there is no change, we don't need to add it to the outputs
            if (totalValues.toLong - amount > 0)
              Seq(lvlOutputForRecipient) :+ lvlOutputForChange
            else
              Seq(lvlOutputForRecipient)
          )
          .withDatum(datum)
      } yield ioTransaction

      override def buildSimpleGroupMintingTransaction(
        walletStateApi: WalletStateAlgebra[F],
        bifrostRpc:     BifrostQueryAlgebra[F],
        groupPolicy:    GroupPolicy,
        lock:           Lock,
        quantity:       Int128
      ): F[Either[BuilderError, IoTransaction]] =
        for {
          stxoRes <- buildRegistrationStxo(walletStateApi, bifrostRpc, groupPolicy.registrationUtxo)
          utxo    <- groupOutput(lock, quantity, groupPolicy.computeId)
          datum   <- datum()
        } yield stxoRes match {
          case Right(stxo) =>
            IoTransaction(
              inputs = Seq(stxo),
              outputs = Seq(utxo),
              datum = datum,
              groupPolicies = Seq(Datum.GroupPolicy(groupPolicy))
            ).asRight
          case Left(err) => err.asLeft
        }

      private def buildRegistrationStxo(
        walletStateApi:   WalletStateAlgebra[F],
        bifrostRpc:       BifrostQueryAlgebra[F],
        registrationUtxo: TransactionOutputAddress
      ): F[Either[BuilderError, SpentTransactionOutput]] =
        (for {
          tx <- EitherT.fromOptionF(
            bifrostRpc.fetchTransaction(registrationUtxo.id),
            BuildStxoError(s"Could not retrieve TX with id ${registrationUtxo.id}")
          )
          utxo <- EitherT.fromOption[F](
            tx.outputs.get(registrationUtxo.index),
            BuildStxoError(
              s"Could not retrieve UTXO with index ${registrationUtxo.index}. Total Outputs: ${tx.outputs.length}"
            )
          )
          lockPredicate <- {
            val address = utxo.address.toBase58()
            EitherT.fromOptionF(
              walletStateApi.getLockByAddress(address),
              BuildStxoError(s"Could not retrieve Lock for Address $address")
            )
          }
          attestation <- EitherT.right[BuilderError](unprovenAttestation(lockPredicate))
        } yield SpentTransactionOutput(address = registrationUtxo, attestation = attestation, value = utxo.value)).value

      private def groupOutput(lock: Lock, quantity: Int128, groupId: GroupId): F[UnspentTransactionOutput] =
        UnspentTransactionOutput(
          LockAddress(
            networkId,
            ledgerId,
            LockId(lock.sizedEvidence.digest.value)
          ),
          Value.defaultInstance.withGroup(Value.Group(groupId = groupId, quantity = quantity))
        ).pure[F]

      override def lvlOutput(
        lockAddress: LockAddress,
        amount:      Int128
      ): F[UnspentTransactionOutput] =
        UnspentTransactionOutput(
          lockAddress,
          Value.defaultInstance.withLvl(Value.LVL(amount))
        ).pure[F]

      override def lockAddress(
        lock: Lock
      ): F[LockAddress] =
        LockAddress(
          networkId,
          ledgerId,
          LockId(lock.sizedEvidence.digest.value)
        ).pure[F]

      override def lvlOutput(
        predicate: Lock.Predicate,
        amount:    Int128
      ): F[UnspentTransactionOutput] =
        UnspentTransactionOutput(
          LockAddress(
            networkId,
            ledgerId,
            LockId(Lock().withPredicate(predicate).sizedEvidence.digest.value)
          ),
          Value.defaultInstance.withLvl(Value.LVL(amount))
        ).pure[F]

      override def datum(): F[Datum.IoTransaction] =
        Datum
          .IoTransaction(
            Event.IoTransaction(Schedule(0, Long.MaxValue, System.currentTimeMillis), SmallData.defaultInstance)
          )
          .pure[F]

      override def unprovenAttestation(
        predicate: Lock.Predicate
      ): F[Attestation] =
        Attestation(
          Attestation.Value.Predicate(
            Attestation.Predicate(
              predicate,
              List.fill(predicate.challenges.length)(Proof())
            )
          )
        ).pure[F]
    }
}
