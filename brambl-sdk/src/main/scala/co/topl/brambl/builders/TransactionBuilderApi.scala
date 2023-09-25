package co.topl.brambl.builders

import cats.Monad
import cats.data.{EitherT, NonEmptyChain}
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
import co.topl.brambl.builders.UserInputValidations.TransactionBuilder._
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.Value.{Asset, Group, LVL, Series, Value => BoxValue}
import co.topl.brambl.syntax.{
  assetAsBoxVal,
  bigIntAsInt128,
  groupAsBoxVal,
  groupPolicyAsGroupPolicySyntaxOps,
  int128AsBigInt,
  longAsInt128,
  lvlAsBoxVal,
  seriesAsBoxVal,
  seriesPolicyAsSeriesPolicySyntaxOps,
  valueToQuantitySyntaxOps,
  valueToTypeIdentifierSyntaxOps,
  AggregateIdentifier,
  AssetType,
  GroupType,
  LvlType,
  SeriesType,
  ValueTypeIdentifier
}
import com.google.protobuf.struct.Struct

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

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
   * @param fee The transaction fee. The txos must contain enough LVLs to satisfy this fee
   * @return A transaction
   */
  def buildLvlTransferTransaction(
    txos:                 Seq[Txo],
    lockPredicateFrom:    Lock.Predicate,
    amount:               Long,
    recipientLockAddress: LockAddress,
    changeLockAddress:    LockAddress,
    fee:                  Long
  ): F[Either[BuilderError, IoTransaction]]

  /**
   * Builds a transaction to transfer a certain amount of Group Constructor Tokens (given by groupId). The transaction
   * will also transfer any other tokens that are encumbered by the same predicate as the Group Constructor Tokens to
   * the change address.
   *
   * @param groupId The Group Identifier of the Group Constructor Tokens to transfer to the recipient
   * @param txos  All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain at least the
   *              necessary quantity (given by amount) of the identified Group Constructor Tokens, else an error will be
   *              returned.
   * @param lockPredicateFrom The Lock Predicate encumbering the txos
   * @param amount The amount of identified Group Constructor Tokens to transfer to the recipient
   * @param recipientLockAddress The LockAddress of the recipient
   * @param changeLockAddress A LockAddress to send the txos that are not going to the recipient
   * @param fee               The transaction fee. The txos must contain enough LVLs to satisfy this fee
   * @return A transaction
   */
  def buildGroupTransferTransaction(
    groupId:              GroupType,
    txos:                 Seq[Txo],
    lockPredicateFrom:    Lock.Predicate,
    amount:               Long,
    recipientLockAddress: LockAddress,
    changeLockAddress:    LockAddress,
    fee:                  Long
  ): F[Either[BuilderError, IoTransaction]]

  /**
   * Builds a transaction to transfer a certain amount of Series Constructor Tokens (given by seriesId). The transaction
   * will also transfer any other tokens that are encumbered by the same predicate as the Series Constructor Tokens to
   * the change address.
   *
   * @param seriesId The Series Identifier of the Series Constructor Tokens to transfer to the recipient
   * @param txos  All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain at least the
   *              necessary quantity (given by amount) of the identified Series Constructor Tokens, else an error will be
   *              returned.
   * @param lockPredicateFrom The Lock Predicate encumbering the txos
   * @param amount The amount of identified Series Constructor Tokens to transfer to the recipient
   * @param recipientLockAddress The LockAddress of the recipient
   * @param changeLockAddress A LockAddress to send the txos that are not going to the recipient
   * @param fee               The transaction fee. The txos must contain enough LVLs to satisfy this fee
   * @return A transaction
   */
  def buildSeriesTransferTransaction(
    seriesId:             SeriesType,
    txos:                 Seq[Txo],
    lockPredicateFrom:    Lock.Predicate,
    amount:               Long,
    recipientLockAddress: LockAddress,
    changeLockAddress:    LockAddress,
    fee:                  Long
  ): F[Either[BuilderError, IoTransaction]]

  /**
   * Builds a transaction to transfer a certain amount of Asset Tokens (given by groupId and/or seriesId). The
   * transaction will also transfer any other tokens that are encumbered by the same predicate as the Asset Tokens to
   * the change address.
   *
   * We currently only support assets with quantity descriptor type LIQUID.
   *
   * @param assetId The Asset Identifier of the Asset Tokens to transfer to the recipient.
   * @param txos  All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain at least the
   *              necessary quantity (given by amount) of the identified Asset Tokens, else an error will be returned.
   * @param lockPredicateFrom The Lock Predicate encumbering the txos
   * @param amount The amount of identified Asset Tokens to transfer to the recipient
   * @param recipientLockAddress The LockAddress of the recipient
   * @param changeLockAddress A LockAddress to send the txos that are not going to the recipient
   * @param fee               The transaction fee. The txos must contain enough LVLs to satisfy this fee
   * @return A transaction
   */
  def buildAssetTransferTransaction(
    assetId:              AssetType,
    txos:                 Seq[Txo],
    lockPredicateFrom:    Lock.Predicate,
    amount:               Long,
    recipientLockAddress: LockAddress,
    changeLockAddress:    LockAddress,
    fee:                  Long
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
   * We currently only support assets with quantity descriptor type LIQUID.
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

  case class UnableToBuildTransaction(causes: Seq[Throwable]) extends BuilderError("Failed to build transaction")

  private def wrapErr(e: Throwable): BuilderError = UnableToBuildTransaction(Seq(e))
  private def wrapErr(e: NonEmptyChain[Throwable]): BuilderError = UnableToBuildTransaction(e.toList)

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
            )((acc, x) => acc + x.transactionOutput.value.value.quantity)
        datum                 <- datum()
        lvlOutputForChange    <- lvlOutput(lockPredicateForChange, totalValues - amount)
        lvlOutputForRecipient <- lvlOutput(recipientLockAddress, amount)
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
            if (totalValues - amount > 0)
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
        changeLockAddress:    LockAddress,
        fee:                  Long
      ): F[Either[BuilderError, IoTransaction]] =
        buildTransferTransaction(txos, lockPredicateFrom, amount, recipientLockAddress, changeLockAddress, LvlType, fee)

      override def buildGroupTransferTransaction(
        groupId:              GroupType,
        txos:                 Seq[Txo],
        lockPredicateFrom:    Lock.Predicate,
        amount:               Long,
        recipientLockAddress: LockAddress,
        changeLockAddress:    LockAddress,
        fee:                  Long
      ): F[Either[BuilderError, IoTransaction]] =
        buildTransferTransaction(txos, lockPredicateFrom, amount, recipientLockAddress, changeLockAddress, groupId, fee)

      override def buildSeriesTransferTransaction(
        seriesId:             SeriesType,
        txos:                 Seq[Txo],
        lockPredicateFrom:    Lock.Predicate,
        amount:               Long,
        recipientLockAddress: LockAddress,
        changeLockAddress:    LockAddress,
        fee:                  Long
      ): F[Either[BuilderError, IoTransaction]] =
        buildTransferTransaction(
          txos,
          lockPredicateFrom,
          amount,
          recipientLockAddress,
          changeLockAddress,
          seriesId,
          fee
        )

      override def buildAssetTransferTransaction(
        assetId:              AssetType,
        txos:                 Seq[Txo],
        lockPredicateFrom:    Lock.Predicate,
        amount:               Long,
        recipientLockAddress: LockAddress,
        changeLockAddress:    LockAddress,
        fee:                  Long
      ): F[Either[BuilderError, IoTransaction]] =
        buildTransferTransaction(txos, lockPredicateFrom, amount, recipientLockAddress, changeLockAddress, assetId, fee)

      private def buildTransferTransaction(
        txos:              Seq[Txo],
        lockPredicateFrom: Lock.Predicate,
        amount:            Long,
        recipientLockAddr: LockAddress,
        changeLockAddr:    LockAddress,
        transferType:      ValueTypeIdentifier,
        fee:               Long
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          fromLockAddr <- EitherT.right(lockAddress(Lock().withPredicate(lockPredicateFrom)))
          _ <- EitherT
            .fromEither[F](validateTransferParams(txos, fromLockAddr, amount, transferType, fee))
            .leftMap(wrapErr)
          stxoAttestation <- EitherT.right(unprovenAttestation(lockPredicateFrom))
          datum           <- EitherT.right(datum())
          stxos           <- buildStxos(txos, stxoAttestation).leftMap(wrapErr)
          utxos <- buildUtxos(txos, transferType.aggregateIdentifier, amount, recipientLockAddr, changeLockAddr, fee)
            .leftMap(wrapErr)
        } yield IoTransaction(inputs = stxos, outputs = utxos, datum = datum)
      ).value

      private def buildStxos(
        txos: Seq[Txo],
        att:  Attestation
      ): EitherT[F, BuilderError, Seq[SpentTransactionOutput]] =
        EitherT.rightT(txos.map(x => SpentTransactionOutput(x.outputAddress, att, x.transactionOutput.value)))

      private def buildUtxos(
        txos:             Seq[Txo],
        transferType:     AggregateIdentifier,
        amount:           Int128,
        recipientAddress: LockAddress,
        changeAddress:    LockAddress,
        fee:              Long
      ): EitherT[F, BuilderError, Seq[UnspentTransactionOutput]] = Try {
        val aggregatedValuesRaw =
          txos.map(_.transactionOutput.value.value).groupMapReduce(_.typeIdentifier.aggregateIdentifier)(identity) {
            // Per grouping, v1&v2 are guaranteed to have the same AggregateIdentifier thus are able to combine quantities
            (v1, v2) => v1.setQuantity(v1.quantity + v2.quantity)
          }
        val aggregatedValues = applyFee(aggregatedValuesRaw, fee)
        val otherVals = (aggregatedValues - transferType).values.toSeq
        val transferVal = aggregatedValues(transferType)
        val changeVal = buildChange(transferVal, transferVal.quantity - amount).toSeq
        UnspentTransactionOutput(recipientAddress, Value.defaultInstance.withValue(transferVal.setQuantity(amount))) +:
        (changeVal ++ otherVals).map(v => UnspentTransactionOutput(changeAddress, Value.defaultInstance.withValue(v)))
      } match {
        case Success(utxos) => EitherT.rightT(utxos)
        case Failure(err)   => EitherT.leftT(BuilderRuntimeError("Failed to build utxos", err))
      }

      // Due to validation, if fee >0, then there must be a lvl in the txos
      private def applyFee(
        groupedValues: Map[AggregateIdentifier, BoxValue],
        fee:           Long
      ): Map[AggregateIdentifier, BoxValue] =
        if (fee > 0) {
          val lvlVal = groupedValues(LvlType.aggregateIdentifier)
          groupedValues.updated(LvlType.aggregateIdentifier, lvlVal.setQuantity(lvlVal.quantity - fee))
        } else groupedValues

      // TODO: Currently we are only supporting LIQUID quantity descriptor type
      // We cannot split an IMMUTABLE or ACCUMULATOR quantity type
      private def buildChange(inValue: BoxValue, changeQuantity: Int128): Option[BoxValue] =
        // User validation guarantees that the following will return None for IMMUTABLE or ACCUMULATOR quantity type
        if (changeQuantity > 0) inValue.setQuantity(changeQuantity).some else None

      override def buildSimpleGroupMintingTransaction(
        registrationTxo:              Txo,
        registrationLock:             Lock.Predicate,
        groupPolicy:                  GroupPolicy,
        quantityToMint:               Int128,
        mintedConstructorLockAddress: LockAddress
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          registrationLockAddr <- EitherT.right[BuilderError](lockAddress(Lock().withPredicate(registrationLock)))
          _ <- EitherT
            .fromEither[F](
              validateConstructorMintingParams(
                registrationTxo,
                registrationLockAddr,
                groupPolicy.registrationUtxo,
                quantityToMint
              )
            )
            .leftMap(wrapErr)
          stxoAttestation <- EitherT.right[BuilderError](unprovenAttestation(registrationLock))
          stxos           <- buildStxos(Seq(registrationTxo), stxoAttestation).leftMap(wrapErr)
          datum           <- EitherT.right[BuilderError](datum())
          utxoMinted <- EitherT.right[BuilderError](
            groupOutput(mintedConstructorLockAddress, quantityToMint, groupPolicy.computeId, groupPolicy.fixedSeries)
          )
        } yield IoTransaction(
          inputs = stxos,
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
          _ <- EitherT
            .fromEither[F](
              validateConstructorMintingParams(
                registrationTxo,
                registrationLockAddr,
                seriesPolicy.registrationUtxo,
                quantityToMint
              )
            )
            .leftMap(wrapErr)
          stxoAttestation <- EitherT.right[BuilderError](unprovenAttestation(registrationLock))
          stxos           <- buildStxos(Seq(registrationTxo), stxoAttestation).leftMap(wrapErr)
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
          inputs = stxos,
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
          _ <- EitherT
            .fromEither[F](
              validateAssetMintingParams(
                mintingStatement,
                groupTxo,
                seriesTxo,
                groupLockAddr,
                seriesLockAddr
              )
            )
            .leftMap(wrapErr)
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
            val inputQuantity = seriesToken.quantity
            val outputQuantity: BigInt =
              if (seriesToken.tokenSupply.isEmpty) inputQuantity
              else inputQuantity - (mintingStatement.quantity / seriesToken.tokenSupply.get)
            if (outputQuantity > 0)
              EitherT.right[BuilderError](
                seriesOutput(
                  seriesTxo.transactionOutput.address,
                  outputQuantity,
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
      ): F[UnspentTransactionOutput] =
        UnspentTransactionOutput(
          lockAddress,
          Value.defaultInstance.withAsset(
            Value.Asset(
              groupId = groupId.some,
              seriesId = seriesId.some,
              groupAlloy = None,
              seriesAlloy = None,
              quantity = quantity,
              fungibility = fungibilityType,
              quantityDescriptor = quantityDescriptorType,
              ephemeralMetadata = metadata,
              commitment = commitment
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
