package co.topl.brambl.builders

import cats.Monad
import cats.data.{Chain, EitherT, NonEmptyChain, Validated, ValidatedNec}
import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.models.{Datum, Event, GroupId, LockAddress, LockId, SeriesId, TransactionOutputAddress}
import co.topl.brambl.models.box.{
  AssetMintingStatement,
  Attestation,
  FungibilityType,
  Lock,
  QuantityDescriptorType,
  Value
}
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.genus.services.Txo
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof, SmallData}
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances.{groupIdentifierImmutable, seriesIdValueImmutable, _}
import co.topl.brambl.common.ContainsEvidence.merkleRootFromBlake2bEvidence
import cats.implicits._
import co.topl.brambl.builders.TransactionBuilderApi.UserInputValidations._
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.Value.{Asset, Group, LVL, Series}
import co.topl.brambl.syntax.{
  bigIntAsInt128SyntaxOps,
  groupPolicyAsGroupPolicySyntaxOps,
  int128Ops,
  seriesPolicyAsSeriesPolicySyntaxOps
}
import com.google.protobuf.struct.Struct

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
   * Builds a group constructor unspent transaction output for the given parameters
   * @param lockAddress The lock address to use to build the group constructor output
   * @param quantity The quantity to use to build the group constructor output
   * @param groupId The group id to use to build the group constructor output
   * @param fixedSeries The fixed series to use to build the group constructor output
   * @return An unspent transaction output containing group constructor tokens
   */
  def groupOutput(
    lockAddress: LockAddress,
    quantity:    Int128,
    groupId:     GroupId,
    fixedSeries: Option[SeriesId]
  ): F[UnspentTransactionOutput]

  /**
   * Builds a series constructor unspent transaction output for the given parameters
   * @param lockAddress The lock address to use to build the series constructor output
   * @param quantity The quantity to use to build the series constructor output
   * @param seriesId The series id to use to build the series constructor output
   * @param tokenSupply The token supply to use to build the series constructor output
   * @param fungibility The fungibility type to use to build the series constructor output
   * @param quantityDescriptor The quantity descriptor type to use to build the series constructor output
   * @return
   */
  def seriesOutput(
    lockAddress:        LockAddress,
    quantity:           Int128,
    seriesId:           SeriesId,
    tokenSupply:        Option[Int],
    fungibility:        FungibilityType,
    quantityDescriptor: QuantityDescriptorType
  ): F[UnspentTransactionOutput]

  /**
   * Builds an asset unspent transaction output for the given parameters
   * @param lockAddress The lock address to use to build the asset output
   * @param quantity The quantity to use to build the asset output
   * @param groupId The group id to use to build the asset output
   * @param seriesId The series id to use to build the asset output
   * @param fungibilityType The fungibility type to use to build the asset output
   * @param quantityDescriptorType The quantity descriptor type to use to build the asset output
   * @param metadata The metadata to use to build the asset output
   * @param commitment The commitment to use to build the asset output
   * @return An unspent transaction output containing asset tokens
   */
  def assetOutput(
    lockAddress:            LockAddress,
    quantity:               Int128,
    groupId:                GroupId,
    seriesId:               SeriesId,
    fungibilityType:        FungibilityType,
    quantityDescriptorType: QuantityDescriptorType,
    metadata:               Option[Struct],
    commitment:             Option[ByteString]
  ): F[UnspentTransactionOutput]

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
   * Builds a transaction to transfer a certain amount of LVLs. The transaction will also transfer any other tokens that
   * are encumbered by the same predicate as the LVLs to the change address.
   *
   * @param txos  All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain at least the
   *              necessary quantity (given by amount) of LVLs, else an error will be returned.
   * @param lockPredicateFrom The Lock Predicate encumbering the txos
   * @param amount The amount of LVLs to transfer to the recipient
   * @param recipientLockAddress The LockAddress of the recipient
   * @param changeLockAddress A LockAddress to send the txos that are not going to the recipient
   */
  def buildLvlTransferTransaction(
    txos:                 Seq[Txo],
    lockPredicateFrom:    Lock.Predicate,
    amount:               Long,
    recipientLockAddress: LockAddress,
    changeLockAddress:    LockAddress
  ): F[Either[BuilderError, IoTransaction]]

  /**
   * Builds a transaction to transfer a certain amount of Group Constructor Tokens (given by groupId). The transaction
   * will also transfer any other tokens that are encumbered by the same predicate as the Group Constructor Tokens to
   * the change address.
   *
   * @param groupId The GroupId of the Group Constructor Tokens to transfer to the recipient
   * @param txos  All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain at least the
   *              necessary quantity (given by amount) of the identified Group Constructor Tokens, else an error will be
   *              returned.
   * @param lockPredicateFrom The Lock Predicate encumbering the txos
   * @param amount The amount of identified Group Constructor Tokens to transfer to the recipient
   * @param recipientLockAddress The LockAddress of the recipient
   * @param changeLockAddress A LockAddress to send the txos that are not going to the recipient
   */
  def buildGroupTransferTransaction(
    groupId:              GroupId,
    txos:                 Seq[Txo],
    lockPredicateFrom:    Lock.Predicate,
    amount:               Long,
    recipientLockAddress: LockAddress,
    changeLockAddress:    LockAddress
  ): F[Either[BuilderError, IoTransaction]]

  /**
   * Builds a transaction to transfer a certain amount of Series Constructor Tokens (given by seriesId). The transaction
   * will also transfer any other tokens that are encumbered by the same predicate as the Series Constructor Tokens to
   * the change address.
   *
   * @param seriesId The SeriesId of the Series Constructor Tokens to transfer to the recipient
   * @param txos  All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain at least the
   *              necessary quantity (given by amount) of the identified Series Constructor Tokens, else an error will be
   *              returned.
   * @param lockPredicateFrom The Lock Predicate encumbering the txos
   * @param amount The amount of identified Series Constructor Tokens to transfer to the recipient
   * @param recipientLockAddress The LockAddress of the recipient
   * @param changeLockAddress A LockAddress to send the txos that are not going to the recipient
   */
  def buildSeriesTransferTransaction(
    seriesId:             SeriesId,
    txos:                 Seq[Txo],
    lockPredicateFrom:    Lock.Predicate,
    amount:               Long,
    recipientLockAddress: LockAddress,
    changeLockAddress:    LockAddress
  ): F[Either[BuilderError, IoTransaction]]

  /**
   * Builds a transaction to transfer a certain amount of Asset Tokens (given by groupId and/or seriesId). The
   * transaction will also transfer any other tokens that are encumbered by the same predicate as the Asset Tokens to
   * the change address.
   *
   * We currently only support assets with fungibility type GROUP_AND_SERIES.
   *
   * @param groupId The GroupId of the Asset Tokens (if fungibility is type GROUP or GROUP_AND_SERIES) to transfer to
   *                the recipient. If fungibility is type SERIES, this parameter is ignored.
   * @param seriesId The SeriesId of the Asset Tokens (if fungibility is type SERIES or GROUP_AND_SERIES) to transfer to
   *                 the recipient. If fungibility is type GROUP, this parameter is ignored.
   * @param txos  All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain at least the
   *              necessary quantity (given by amount) of the identified Asset Tokens, else an error will be returned.
   * @param lockPredicateFrom The Lock Predicate encumbering the txos
   * @param amount The amount of identified Asset Tokens to transfer to the recipient
   * @param recipientLockAddress The LockAddress of the recipient
   * @param changeLockAddress A LockAddress to send the txos that are not going to the recipient
   */
  def buildAssetTransferTransaction(
    groupId:              Option[GroupId],
    seriesId:             Option[SeriesId],
    txos:                 Seq[Txo],
    lockPredicateFrom:    Lock.Predicate,
    amount:               Long,
    recipientLockAddress: LockAddress,
    changeLockAddress:    LockAddress
  ): F[Either[BuilderError, IoTransaction]]

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
   * We currently only support assets with fungibility type GROUP_AND_SERIES.
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
    ephemeralMetadata:      Option[Struct],
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

    def fixedSeriesMatch(
      testValue:     Option[SeriesId],
      expectedValue: Option[SeriesId]
    ): ValidatedNec[UserInputError, Unit] =
      (testValue, expectedValue) match {
        case (Some(fixedSeries), Some(expectedSeries)) =>
          Validated.condNec(
            fixedSeries == expectedSeries,
            (),
            UserInputError(s"fixedSeries does not match provided Series ID")
          )
        case _ => ().validNec[UserInputError]
      }

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

    def positiveQuantity(testQuantity: Option[Int128], testLabel: String): ValidatedNec[UserInputError, Unit] =
      Validated.condNec(
        testQuantity.isEmpty || BigInt(testQuantity.get.value.toByteArray) > BigInt(0),
        (),
        UserInputError(s"$testLabel must be positive")
      )

    def validMintingSupply(
      testQuantity:   Int128,
      seriesTokenOpt: Option[Series],
      testLabel:      String
    ): ValidatedNec[UserInputError, Unit] =
      (testQuantity, seriesTokenOpt) match {
        case (d, Some(Series(_, a, Some(tokenSupply), _, _, _))) =>
          val desiredQ = BigInt(d.value.toByteArray)
          val availableQ = BigInt(a.value.toByteArray)
          if (desiredQ % tokenSupply != 0)
            UserInputError(s"$testLabel must be a multiple of token supply").invalidNec[Unit]
          else if (desiredQ > availableQ * tokenSupply)
            UserInputError(s"$testLabel must be less than total token supply available.").invalidNec[Unit]
          else ().validNec[UserInputError]
        case _ => ().validNec[UserInputError]
      }

    def fungibilityType(testType: Option[FungibilityType]): ValidatedNec[UserInputError, Unit] =
      Validated.condNec(
        testType.isEmpty || testType.get == FungibilityType.GROUP_AND_SERIES,
        (),
        UserInputError(s"Unsupported fungibility type. We currently only support GROUP_AND_SERIES")
      )
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

      override def buildLvlTransferTransaction(
        txos:                 Seq[Txo],
        lockPredicateFrom:    Lock.Predicate,
        amount:               Long,
        recipientLockAddress: LockAddress,
        changeLockAddress:    LockAddress
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          _ <- EitherT.fromEither[F](
            validatePlaceholder()
              .leftMap[BuilderError](errs =>
                UnableToBuildTransaction("Unable to build transaction to transfer LVLs", errs.toList)
              )
          )
          stxoAttestation <- EitherT.right[BuilderError](unprovenAttestation(lockPredicateFrom))
          datum           <- EitherT.right[BuilderError](datum())
          (otherTxoValues, lvlValues) = txos
            .map(_.transactionOutput.value)
            .partitionMap(value => value.value.lvl.toRight(value))
          transferredUtxo <- EitherT.right[BuilderError](
            lvlOutput(recipientLockAddress, BigInt(amount).toInt128)
          )
          totalQuantity = lvlValues.foldLeft(BigInt(0))((acc, lvl) => acc + lvl.quantity.toBigInt)
          change <- {
            val leftover = totalQuantity - BigInt(amount)
            val changeOutput =
              if (leftover > BigInt(0))
                lvlOutput(changeLockAddress, leftover.toInt128).map(Seq(_))
              else Seq.empty[UnspentTransactionOutput].pure[F]
            EitherT.right[BuilderError](changeOutput)
          }
        } yield IoTransaction(
          inputs = txos.map(x =>
            SpentTransactionOutput(
              x.outputAddress,
              stxoAttestation,
              x.transactionOutput.value
            )
          ),
          outputs = transferredUtxo +: change ++: groupAndBuildOutputs(otherTxoValues, changeLockAddress),
          datum = datum
        )
      ).value

      private def groupAndBuildOutputs(values: Seq[Value], lockAddress: LockAddress): Seq[UnspentTransactionOutput] =
        mergeValues(values.map(_.value))
          .map(v => UnspentTransactionOutput(lockAddress, Value.defaultInstance.withValue(v)))

      private def mergeValues(values: Seq[Value.Value]): Seq[Value.Value] =
        groupValues(values).map(_.reduceLeft(merge2Values))

      // TODO: This needs to be updated when we want to support multiple fungibility types
      private def merge2Values[T <: Value.Value](value1: T, value2: T): Value.Value = (value1, value2) match {
        case (Value.Value.Lvl(value @ LVL(quantity1, _)), Value.Value.Lvl(LVL(quantity2, _))) =>
          Value.Value.Lvl(value.copy(quantity = quantity1 + quantity2))
        case (Value.Value.Group(value @ Group(_, quantity1, _, _)), Value.Value.Group(Group(_, quantity2, _, _))) =>
          Value.Value.Group(value.copy(quantity = quantity1 + quantity2))
        case (
              Value.Value.Series(value @ Series(_, quantity1, _, _, _, _)),
              Value.Value.Series(Series(_, quantity2, _, _, _, _))
            ) =>
          Value.Value.Series(value.copy(quantity = quantity1 + quantity2))
        case (
              Value.Value.Asset(value @ Asset(_, _, quantity1, _, _, _, _, _, _, _)),
              Value.Value.Asset(Asset(_, _, quantity2, _, _, _, _, _, _, _))
            ) =>
          Value.Value.Asset(value.copy(quantity = quantity1 + quantity2))
      }

      // TODO: This needs to be updated when we want to support multiple fungibility types
      private def groupValues(values: Seq[Value.Value]): Seq[Seq[Value.Value]] = values
        .groupBy {
          case Value.Value.Lvl(LVL(_, _))                          => "lvl"
          case Value.Value.Group(Group(groupId, _, _, _))          => s"group_$groupId"
          case Value.Value.Series(Series(seriesId, _, _, _, _, _)) => s"series_$seriesId"
          case Value.Value.Asset(Asset(Some(groupId), Some(seriesId), _, _, _, _, _, _, _, _)) =>
            s"asset_${groupId}_${seriesId}"
        }
        .values
        .toSeq

      override def buildGroupTransferTransaction(
        groupId:              GroupId,
        txos:                 Seq[Txo],
        lockPredicateFrom:    Lock.Predicate,
        amount:               Long,
        recipientLockAddress: LockAddress,
        changeLockAddress:    LockAddress
      ): F[Either[BuilderError, IoTransaction]] = ???

      override def buildSeriesTransferTransaction(
        seriesId:             SeriesId,
        txos:                 Seq[Txo],
        lockPredicateFrom:    Lock.Predicate,
        amount:               Long,
        recipientLockAddress: LockAddress,
        changeLockAddress:    LockAddress
      ): F[Either[BuilderError, IoTransaction]] = ???

      override def buildAssetTransferTransaction(
        groupId:              Option[GroupId],
        seriesId:             Option[SeriesId],
        txos:                 Seq[Txo],
        lockPredicateFrom:    Lock.Predicate,
        amount:               Long,
        recipientLockAddress: LockAddress,
        changeLockAddress:    LockAddress
      ): F[Either[BuilderError, IoTransaction]] = ???

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
            groupOutput(mintedConstructorLockAddress, quantityToMint, groupPolicy.computeId, groupPolicy.fixedSeries)
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
            seriesOutput(
              mintedConstructorLockAddress,
              quantityToMint,
              seriesPolicy.computeId,
              seriesPolicy.tokenSupply,
              seriesPolicy.fungibility,
              seriesPolicy.quantityDescriptor
            )
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
        ephemeralMetadata:      Option[Struct],
        commitment:             Option[Array[Byte]]
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          groupLockAddr  <- EitherT.right[BuilderError](lockAddress(Lock().withPredicate(groupLock)))
          seriesLockAddr <- EitherT.right[BuilderError](lockAddress(Lock().withPredicate(seriesLock)))
          _ <- EitherT.fromEither[F](
            validateAssetMintingParams(
              mintingStatement,
              groupTxo,
              seriesTxo,
              groupLockAddr,
              seriesLockAddr
            )
              .leftMap[BuilderError](errs =>
                UnableToBuildTransaction("Unable to build transaction to mint asset tokens", errs.toList)
              )
          )
          groupAttestation  <- EitherT.right[BuilderError](unprovenAttestation(groupLock))
          seriesAttestation <- EitherT.right[BuilderError](unprovenAttestation(seriesLock))
          datum             <- EitherT.right[BuilderError](datum())
          groupToken = groupTxo.transactionOutput.value.value.group.get
          seriesToken = seriesTxo.transactionOutput.value.value.series.get
          utxoMinted <- EitherT.right[BuilderError](
            assetOutput(
              mintedAssetLockAddress,
              mintingStatement.quantity,
              groupToken.groupId,
              seriesToken.seriesId,
              seriesToken.fungibility,
              seriesToken.quantityDescriptor,
              ephemeralMetadata,
              commitment.map(ByteString.copyFrom)
            )
          )
          groupOutput <- EitherT.right[BuilderError](
            groupOutput(
              groupTxo.transactionOutput.address,
              groupToken.quantity,
              groupToken.groupId,
              groupToken.fixedSeries
            )
          )
          seriesOutputSeq <- {
            val inputQuantity = BigInt(seriesToken.quantity.value.toByteArray)
            val outputQuantity: BigInt =
              if (seriesToken.tokenSupply.isEmpty) inputQuantity
              else {
                val toMint = BigInt(mintingStatement.quantity.value.toByteArray)
                val supply = seriesToken.tokenSupply.get
                val numUsed = toMint / supply
                inputQuantity - numUsed
              }
            if (outputQuantity > BigInt(0))
              EitherT.right[BuilderError](
                seriesOutput(
                  seriesTxo.transactionOutput.address,
                  Int128(
                    ByteString.copyFrom(
                      outputQuantity.toByteArray
                    )
                  ),
                  seriesToken.seriesId,
                  seriesToken.tokenSupply,
                  seriesToken.fungibility,
                  seriesToken.quantityDescriptor
                )
                  .map(utxo => Seq(utxo))
              )
            else EitherT.rightT[F, BuilderError](Seq.empty[UnspentTransactionOutput])
          }
        } yield IoTransaction(
          inputs = Seq(
            SpentTransactionOutput(
              groupTxo.outputAddress,
              groupAttestation,
              groupTxo.transactionOutput.value
            ),
            SpentTransactionOutput(
              seriesTxo.outputAddress,
              seriesAttestation,
              seriesTxo.transactionOutput.value
            )
          ),
          outputs = seriesOutputSeq :+ utxoMinted :+ groupOutput,
          datum = datum,
          mintingStatements = Seq(mintingStatement)
        )
      ).value

      private def validatePlaceholder(): Either[NonEmptyChain[UserInputError], Unit] = ???

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
          positiveQuantity(quantityToMint.some, "quantityToMint")
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
          positiveQuantity(mintingStatement.quantity.some, "quantity to mint"),
          positiveQuantity(
            seriesTxo.transactionOutput.value.value.series.map(_.quantity),
            "quantity of input series constructor tokens"
          ),
          validMintingSupply(
            mintingStatement.quantity,
            seriesTxo.transactionOutput.value.value.series,
            "quantity to mint"
          ),
          fixedSeriesMatch(
            groupTxo.transactionOutput.value.value.group.flatMap(_.fixedSeries),
            seriesTxo.transactionOutput.value.value.series.map(_.seriesId)
          ),
          fungibilityType(seriesTxo.transactionOutput.value.value.series.map(_.fungibility))
        ).fold.toEither

      override def groupOutput(
        lockAddress: LockAddress,
        quantity:    Int128,
        groupId:     GroupId,
        fixedSeries: Option[SeriesId]
      ): F[UnspentTransactionOutput] =
        UnspentTransactionOutput(
          lockAddress,
          Value.defaultInstance.withGroup(
            Value.Group(groupId = groupId, quantity = quantity, fixedSeries = fixedSeries)
          )
        ).pure[F]

      override def seriesOutput(
        lockAddress:        LockAddress,
        quantity:           Int128,
        seriesId:           SeriesId,
        tokenSupply:        Option[Int],
        fungibility:        FungibilityType,
        quantityDescriptor: QuantityDescriptorType
      ): F[UnspentTransactionOutput] =
        UnspentTransactionOutput(
          lockAddress,
          Value.defaultInstance.withSeries(
            Value.Series(
              seriesId = seriesId,
              quantity = quantity,
              tokenSupply = tokenSupply,
              quantityDescriptor = quantityDescriptor,
              fungibility = fungibility
            )
          )
        ).pure[F]

      override def assetOutput(
        lockAddress:            LockAddress,
        quantity:               Int128,
        groupId:                GroupId,
        seriesId:               SeriesId,
        fungibilityType:        FungibilityType,
        quantityDescriptorType: QuantityDescriptorType,
        metadata:               Option[Struct],
        commitment:             Option[ByteString]
      ): F[UnspentTransactionOutput] = {
        val groupSeriesValues = fungibilityType match {
          case FungibilityType.GROUP_AND_SERIES => (groupId.some, seriesId.some, None, None)
          case FungibilityType.SERIES =>
            (
              None,
              seriesId.some,
              merkleRootFromBlake2bEvidence(groupIdentifierImmutable).sizedEvidence(List(groupId)).digest.value.some,
              None
            )
          case FungibilityType.GROUP =>
            (
              groupId.some,
              None,
              None,
              merkleRootFromBlake2bEvidence(seriesIdValueImmutable).sizedEvidence(List(seriesId)).digest.value.some
            )
        }
        UnspentTransactionOutput(
          lockAddress,
          Value.defaultInstance.withAsset(
            Value.Asset(
              groupId = groupSeriesValues._1,
              seriesId = groupSeriesValues._2,
              groupAlloy = groupSeriesValues._3,
              seriesAlloy = groupSeriesValues._4,
              quantity = quantity,
              fungibility = fungibilityType,
              quantityDescriptor = quantityDescriptorType,
              ephemeralMetadata = metadata,
              commitment = commitment
            )
          )
        ).pure[F]
      }

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
