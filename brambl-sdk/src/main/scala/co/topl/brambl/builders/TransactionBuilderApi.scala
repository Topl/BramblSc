package co.topl.brambl.builders

import cats.Monad
import cats.data.{Chain, EitherT, NonEmptyChain, Validated, ValidatedNec}
import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.models.{Datum, Event, GroupId, LockAddress, LockId, TransactionOutputAddress}
import co.topl.brambl.models.box.{AssetMintingStatement, Attestation, Lock, Value}
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.genus.services.Txo
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof, SmallData}
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._
import cats.implicits._
import co.topl.brambl.builders.TransactionBuilderApi.UserInputValidations._
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.Value.Series
import co.topl.brambl.syntax.{groupPolicyAsGroupPolicySyntaxOps, seriesPolicyAsSeriesPolicySyntaxOps}
import io.circe.Json

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

  /**
   * Builds a simple transaction to mint asset tokens.
   * If successful, the transaction will have two inputs (the group and series constructor token UTXOs) and 2-3 outputs.
   * The first output will be the minted asset tokens. The second output will be the group constructor tokens (since
   * they are never burned). The potential third output will be the series constructor tokens that were not burned.
   *
   * @param mintingStatement      The minting statement that specifies the asset to mint.
   * @param groupTxo              The TXO that corresponds to the groupTokenUtxo (in the asset minting statement) to use
   *                              as an input in this transaction. This TXO must contain group constructor tokens, else
   *                              an error will be returned. None of this TXO will be burned.
   * @param seriesTxo             The TXO that corresponds to the seriesTokenUtxo (in the asset minting statement) to
   *                              use as an input in this transaction. This TXO must contain series constructor tokens,
   *                              else an error will be returned. If the "tokenSupply" field in the series constructor
   *                              tokens is present, then the quantity of asset tokens to mint has to be a multiple of
   *                              this field, else an error will be returned. In this case, minting each multiple of
   *                              "tokenSupply" quantity of assets will burn a single series constructor token.
   * @param groupLock             The Predicate Lock that encumbers the funds in the groupTxo. This will be used in the
   *                              attestation of the groupTxo input.
   * @param seriesLock            The Predicate Lock that encumbers the funds in the seriesTxo. This will be used in the
   *                              attestation of the seriesTxo input.
   * @param mintedAssetLockAddress The LockAddress to send the minted asset tokens to.
   * @param ephemeralMetadata     Optional ephemeral metadata to include in the minted asset tokens.
   * @param commitment            Optional commitment to include in the minted asset tokens.
   * @return An unproven asset minting transaction if possible. Else, an error
   */
  def buildSimpleAssetMintingTransaction(
    mintingStatement:       AssetMintingStatement,
    groupTxo:               Txo,
    seriesTxo:              Txo,
    groupLock:              Lock.Predicate,
    seriesLock:             Lock.Predicate,
    mintedAssetLockAddress: LockAddress,
    ephemeralMetadata:      Option[Json],
    commitment:             Option[Array[Byte]]
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
  case class UnableToBuildTransaction(message: String, causes: List[Throwable] = List()) extends BuilderError(message)

  object UserInputValidations {

    def txoAddressMatch(
      testAddr:      TransactionOutputAddress,
      expectedAddr:  TransactionOutputAddress,
      testLabel:     String,
      expectedLabel: String
    ): ValidatedNec[UserInputError, Unit] =
      Validated.condNec(testAddr == expectedAddr, (), UserInputError(s"$testLabel does not match $expectedLabel"))

    def isLvls(testValue: Value.Value, testLabel: String): ValidatedNec[UserInputError, Unit] =
      Validated.condNec(testValue.isLvl, (), UserInputError(s"$testLabel does not contain LVLs"))

    def isGroup(testValue: Value.Value, testLabel: String): ValidatedNec[UserInputError, Unit] =
      Validated.condNec(testValue.isGroup, (), UserInputError(s"$testLabel does not contain Group Constructor Tokens"))

    def isSeries(testValue: Value.Value, testLabel: String): ValidatedNec[UserInputError, Unit] =
      Validated.condNec(
        testValue.isSeries,
        (),
        UserInputError(s"$testLabel does not contain Series Constructor Tokens")
      )

    def inputLockMatch(
      testAddr:      LockAddress,
      expectedAddr:  LockAddress,
      testLabel:     String,
      expectedLabel: String
    ): ValidatedNec[UserInputError, Unit] =
      Validated.condNec(
        testAddr == expectedAddr,
        (),
        UserInputError(s"$testLabel does not correspond to $expectedLabel")
      )

    def positiveQuantity(testQuantity: Int128, testLabel: String): ValidatedNec[UserInputError, Unit] =
      Validated.condNec(
        BigInt(testQuantity.value.toByteArray) > BigInt(0),
        (),
        UserInputError(s"$testLabel must be positive")
      )

    def validMintingSupply(
      testQuantity: Int128,
      seriesToken:  Series,
      testLabel:    String
    ): ValidatedNec[UserInputError, Unit] =
      (testQuantity, seriesToken.tokenSupply, seriesToken.quantity) match {
        case (_, None, _) => ().validNec[UserInputError] // unlimited supply
        case (d, Some(supply), a) =>
          val desiredQ = BigInt(d.value.toByteArray)
          val availableQ = BigInt(a.value.toByteArray)
          if (desiredQ % supply != 0) UserInputError(s"$testLabel must be a multiple of token supply").invalidNec[Unit]
          else if (desiredQ > availableQ * supply)
            UserInputError(s"$testLabel must be less than total token supply available.").invalidNec[Unit]
          else ().validNec[UserInputError]
      }
  }

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
              .leftMap[BuilderError](errs =>
                UnableToBuildTransaction("Unable to build transaction to mint group constructor tokens", errs.toList)
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
          _ <- EitherT.fromEither[F](
            validateConstructorMintingParams(
              registrationTxo,
              registrationLockAddr,
              seriesPolicy.registrationUtxo,
              quantityToMint
            )
              .leftMap[BuilderError](errs =>
                UnableToBuildTransaction("Unable to build transaction to mint series constructor tokens", errs.toList)
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

      override def buildSimpleAssetMintingTransaction(
        mintingStatement:       AssetMintingStatement,
        groupTxo:               Txo,
        seriesTxo:              Txo,
        groupLock:              Lock.Predicate,
        seriesLock:             Lock.Predicate,
        mintedAssetLockAddress: LockAddress,
        ephemeralMetadata:      Option[Json],
        commitment:             Option[Array[Byte]]
      ): F[Either[BuilderError, IoTransaction]] = ???

      /**
       * Validates the parameters for minting group and series constructor tokens
       * If user parameters are invalid, return a UserInputError.
       */
      private def validateConstructorMintingParams(
        registrationTxo:        Txo,
        registrationLockAddr:   LockAddress,
        policyRegistrationUtxo: TransactionOutputAddress,
        quantityToMint:         Int128
      ): Either[NonEmptyChain[UserInputError], Unit] =
        Chain(
          txoAddressMatch(registrationTxo.outputAddress, policyRegistrationUtxo, "registrationTxo", "registrationUtxo"),
          isLvls(registrationTxo.transactionOutput.value.value, "registrationUtxo"),
          inputLockMatch(
            registrationLockAddr,
            registrationTxo.transactionOutput.address,
            "registrationLock",
            "registrationTxo"
          ),
          positiveQuantity(quantityToMint, "quantityToMint")
        ).fold.toEither

      /**
       * Validates the parameters for minting asset tokens
       * If user parameters are invalid, return a UserInputError.
       */
      private def validateAssetMintingParams(
        mintingStatement: AssetMintingStatement,
        groupTxo:         Txo,
        seriesTxo:        Txo,
        groupLockAddr:    LockAddress,
        seriesLockAddr:   LockAddress
      ): Either[NonEmptyChain[UserInputError], Unit] =
        Chain(
          txoAddressMatch(groupTxo.outputAddress, mintingStatement.groupTokenUtxo, "groupTxo", "groupTokenUtxo"),
          txoAddressMatch(seriesTxo.outputAddress, mintingStatement.seriesTokenUtxo, "seriesTxo", "seriesTokenUtxo"),
          isGroup(groupTxo.transactionOutput.value.value, "groupTxo"),
          isSeries(seriesTxo.transactionOutput.value.value, "seriesTxo"),
          inputLockMatch(
            groupLockAddr,
            groupTxo.transactionOutput.address,
            "groupLock",
            "groupTxo"
          ),
          inputLockMatch(
            seriesLockAddr,
            seriesTxo.transactionOutput.address,
            "seriesLock",
            "seriesTxo"
          ),
          positiveQuantity(mintingStatement.quantity, "quantity to mint"),
          validMintingSupply(
            mintingStatement.quantity,
            seriesTxo.transactionOutput.value.value.series.get,
            "quantity to mint"
          )
        ).fold.toEither

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
