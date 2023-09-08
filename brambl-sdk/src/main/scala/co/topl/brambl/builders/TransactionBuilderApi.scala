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
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.syntax.{groupPolicyAsGroupPolicySyntaxOps, seriesPolicyAsSeriesPolicySyntaxOps}

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
   * Builds a simple transaction to mint Group Constructor tokens.
   * If successful, the transaction will have a single input (the registrationUtxo) and a single output (the minted
   * group constructor tokens).
   *
   * @param registrationTxo The TXO that corresponds to the registrationUtxo to use as an input in this transaction.
   *                        This TXO must contain LVLs, else an error will be returned. The entirety of this TXO will
   *                        be used as the fee to mint the series constructor token.
   * @param registrationLock The Predicate Lock that encumbers the funds in the registrationUtxo. This will be used in
   *                         the attestation of the registrationUtxo input.
   * @param groupPolicy The group policy for which we are minting constructor tokens. This group policy specifies a
   *                    registrationUtxo to be used as an input in this transaction.
   * @param quantityToMint The quantity of constructor tokens to mint
   * @param mintedConstructorLockAddress The LockAddress to send the minted constructor tokens to.
   * @return An unproven Group Constructor minting transaction if possible. Else, an error
   */
  def buildSimpleGroupMintingTransaction(
    registrationTxo:              Txo,
    registrationLock:             Lock.Predicate,
    groupPolicy:                  GroupPolicy,
    quantityToMint:               Int128,
    mintedConstructorLockAddress: LockAddress
  ): F[Either[BuilderError, IoTransaction]]

  /**
   * Builds a simple transaction to mint Series Constructor tokens.
   * If successful, the transaction will have a single input (the registrationUtxo) and a single output (the minted
   * series constructor tokens).
   *
   * @param registrationTxo The TXO that corresponds to the registrationUtxo to use as an input in this transaction.
   *                        This TXO must contain LVLs, else an error will be returned. The entirety of this TXO will
   *                        be used as the fee to mint the series constructor token.
   * @param registrationLock The Predicate Lock that encumbers the funds in the registrationUtxo. This will be used in
   *                         the attestation of the registrationUtxo input.
   * @param seriesPolicy The series policy for which we are minting constructor tokens. This series policy specifies a
   *                    registrationUtxo to be used as an input in this transaction.
   * @param quantityToMint The quantity of constructor tokens to mint
   * @param mintedConstructorLockAddress The LockAddress to send the minted constructor tokens to.
   * @return An unproven Series Constructor minting transaction if possible. Else, an error
   */
  def buildSimpleSeriesMintingTransaction(
    registrationTxo:              Txo,
    registrationLock:             Lock.Predicate,
    seriesPolicy:                 SeriesPolicy,
    quantityToMint:               Int128,
    mintedConstructorLockAddress: LockAddress
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

  case class UserInputError(message: String) extends BuilderError(message, null)
  case class UnableToBuildTransaction(message: String, cause: Throwable = null) extends BuilderError(message, cause)

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
        registrationTxo:              Txo,
        registrationLock:             Lock.Predicate,
        groupPolicy:                  GroupPolicy,
        quantityToMint:               Int128,
        mintedConstructorLockAddress: LockAddress
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          registrationLockAddr <- EitherT.right[BuilderError](lockAddress(Lock().withPredicate(registrationLock)))
          _ <- EitherT.fromEither[F](
            validateConstructorMintingParams(
              registrationTxo,
              registrationLockAddr,
              groupPolicy.registrationUtxo,
              quantityToMint
            )
              .leftMap[BuilderError](
                UnableToBuildTransaction("Unable to build transaction to mint group constructor tokens", _)
              )
          )
          stxoAttestation <- EitherT.right[BuilderError](unprovenAttestation(registrationLock))
          datum           <- EitherT.right[BuilderError](datum())
          utxoMinted <- EitherT.right[BuilderError](
            groupOutput(mintedConstructorLockAddress, quantityToMint, groupPolicy.computeId)
          )
        } yield IoTransaction(
          inputs = Seq(
            SpentTransactionOutput(
              registrationTxo.outputAddress,
              stxoAttestation,
              registrationTxo.transactionOutput.value
            )
          ),
          outputs = Seq(utxoMinted),
          datum = datum,
          groupPolicies = Seq(Datum.GroupPolicy(groupPolicy))
        )
      ).value

      override def buildSimpleSeriesMintingTransaction(
        registrationTxo:              Txo,
        registrationLock:             Lock.Predicate,
        seriesPolicy:                 SeriesPolicy,
        quantityToMint:               Int128,
        mintedConstructorLockAddress: LockAddress
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          registrationLockAddr <- EitherT.right[BuilderError](lockAddress(Lock().withPredicate(registrationLock)))
          change <- EitherT.fromEither[F](
            validateConstructorMintingParams(
              registrationTxo,
              registrationLockAddr,
              seriesPolicy.registrationUtxo,
              quantityToMint
            )
              .leftMap[BuilderError](
                UnableToBuildTransaction("Unable to build transaction to mint series constructor tokens", _)
              )
          )
          stxoAttestation <- EitherT.right[BuilderError](unprovenAttestation(registrationLock))
          datum           <- EitherT.right[BuilderError](datum())
          utxoMinted <- EitherT.right[BuilderError](
            seriesOutput(mintedConstructorLockAddress, quantityToMint, seriesPolicy)
          )
        } yield IoTransaction(
          inputs = Seq(
            SpentTransactionOutput(
              registrationTxo.outputAddress,
              stxoAttestation,
              registrationTxo.transactionOutput.value
            )
          ),
          outputs = Seq(utxoMinted),
          datum = datum,
          seriesPolicies = Seq(Datum.SeriesPolicy(seriesPolicy))
        )
      ).value

      /**
       * Validates the parameters for minting group constructor tokens
       * If user parameters are invalid, return a UserInputError.
       */
      private def validateConstructorMintingParams(
        registrationTxo:        Txo,
        registrationLockAddr:   LockAddress,
        policyRegistrationUtxo: TransactionOutputAddress,
        quantityToMint:         Int128
      ): Either[UserInputError, Unit] =
        if (registrationTxo.outputAddress != policyRegistrationUtxo)
          UserInputError("registrationTxo does not match registrationUtxo").asLeft
        else if (!registrationTxo.transactionOutput.value.value.isLvl)
          UserInputError("registrationUtxo does not contain LVLs").asLeft
        else if (registrationLockAddr != registrationTxo.transactionOutput.address)
          UserInputError("registrationLock does not correspond to registrationTxo").asLeft
        else if (BigInt(quantityToMint.value.toByteArray) <= BigInt(0))
          UserInputError("quantityToMint must be positive").asLeft
        else ().asRight

      private def groupOutput(
        lockAddress: LockAddress,
        quantity:    Int128,
        groupId:     GroupId
      ): F[UnspentTransactionOutput] =
        UnspentTransactionOutput(
          lockAddress,
          Value.defaultInstance.withGroup(Value.Group(groupId = groupId, quantity = quantity))
        ).pure[F]

      private def seriesOutput(
        lockAddress: LockAddress,
        quantity:    Int128,
        policy:      SeriesPolicy
      ): F[UnspentTransactionOutput] =
        UnspentTransactionOutput(
          lockAddress,
          Value.defaultInstance.withSeries(
            Value.Series(
              seriesId = policy.computeId,
              quantity = quantity,
              tokenSupply = policy.tokenSupply,
              quantityDescriptor = policy.quantityDescriptor,
              fungibility = policy.fungibility
            )
          )
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
