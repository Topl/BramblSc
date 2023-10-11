package co.topl.brambl.builders

import cats.Monad
import cats.data.EitherT
import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.models.{Datum, Event, GroupId, LockAddress, LockId, SeriesId}
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
import co.topl.brambl.common.ContainsImmutable.instances._
import cats.implicits._
import co.topl.brambl.builders.UserInputValidations.TransactionBuilder._
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.Value.{Value => BoxValue}
import co.topl.brambl.syntax.{
  bigIntAsInt128,
  groupPolicyAsGroupPolicySyntaxOps,
  int128AsBigInt,
  longAsInt128,
  seriesPolicyAsSeriesPolicySyntaxOps,
  valueToQuantitySyntaxOps,
  valueToTypeIdentifierSyntaxOps,
  LvlType,
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
   * Builds a transaction to transfer the ownership of tokens (optionally identified by tokenIdentifier). If
   * tokenIdentifier is provided, only the TXOs matching the identifier will go to the recipient. If it is None, then
   * all tokens provided in txos will go to the recipient. Any remaining tokens in txos that are not transferred to the
   * recipient will be transferred to the change address.
   *
   * TODO: Add support for TOPLs and UpdateProposals
   * @note Currently TOPLs and UpdateProposal values are not supported in the txos. This will be added in TSDK-610
   * @note Currently TOPLs and UpdateProposal values are not supported in tokenIdentifier. This will be added in TSDK-610
   *
   * @param txos All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain some token
   *             matching tokenIdentifier (if it is provided) and at least the quantity of LVLs to satisfy the fee, else
   *             an error will be returned.
   * @param lockPredicateFrom The Lock Predicate encumbering the txos
   * @param recipientLockAddress The LockAddress of the recipient
   * @param changeLockAddress A LockAddress to send the tokens that are not going to the recipient
   * @param fee The fee to pay for the transaction. The txos must contain enough LVLs to satisfy this fee
   * @param tokenIdentifier An optional token identifier to denote the type of token to transfer to the recipient. If
   *                        None, all tokens in txos will be transferred to the recipient and changeLockAddress will be
   *                        ignored.
   * @return An unproven transaction
   */
  def buildTransferAllTransaction(
    txos:                 Seq[Txo],
    lockPredicateFrom:    Lock.Predicate,
    recipientLockAddress: LockAddress,
    changeLockAddress:    LockAddress,
    fee:                  Long,
    tokenIdentifier:      Option[ValueTypeIdentifier] = None
  ): F[Either[BuilderError, IoTransaction]]

  /**
   * Builds a transaction to transfer a certain amount of a specified Token (given by tokenIdentifier). The transaction
   * will also transfer any other tokens (in the txos) that are encumbered by the same predicate to the change address.
   *
   * This function only supports transferring a specific amount of assets (via tokenIdentifier) if their quantity
   * descriptor type is LIQUID.
   *
   * TODO: Add support for TOPLs and UpdateProposals
   * @note Currently TOPLs and UpdateProposal values are not supported in the txos. This will be added in TSDK-610
   * @note Currently TOPLs and UpdateProposal values are not supported in tokenIdentifier. This may or may not be added
   *       in TSDK-610 depending if these values can be aggregated and deaggregated by default. Pending discussion.
   *
   * @param tokenIdentifier The Token Identifier denoting the type of token to transfer to the recipient. If this denotes
   *                        an Asset Token, the quantity descriptor type must be LIQUID, else an error will be returned.
   * @param txos All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain at least the
   *             necessary quantity (given by amount) of the identified Token and at least the quantity of LVLs to
   *             satisfy the fee, else an error will be returned.
   * @param lockPredicateFrom The Lock Predicate encumbering the txos
   * @param amount The amount of identified Token to transfer to the recipient
   * @param recipientLockAddress The LockAddress of the recipient
   * @param changeLockAddress A LockAddress to send the tokens that are not going to the recipient
   * @param fee              The transaction fee. The txos must contain enough LVLs to satisfy this fee
   * @return An unproven transaction
   */
  def buildTransferAmountTransaction(
    tokenIdentifier:      ValueTypeIdentifier,
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

      override def buildTransferAllTransaction(
        txos:              Seq[Txo],
        lockPredicateFrom: Lock.Predicate,
        recipientLockAddr: LockAddress,
        changeLockAddr:    LockAddress,
        fee:               Long,
        tokenIdentifier:   Option[ValueTypeIdentifier]
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          fromLockAddr <- EitherT.right(lockAddress(Lock().withPredicate(lockPredicateFrom)))
          _ <- EitherT
            .fromEither[F](validateTransferAllParams(txos, fromLockAddr, fee, tokenIdentifier))
            .leftMap(errs => UserInputErrors(errs.toList))
          stxoAttestation <- EitherT.right(unprovenAttestation(lockPredicateFrom))
          datum           <- EitherT.right(datum())
          stxos           <- buildStxos(txos, stxoAttestation)
          utxos           <- buildUtxos(txos, tokenIdentifier, None, recipientLockAddr, changeLockAddr, fee)
        } yield IoTransaction(inputs = stxos, outputs = utxos, datum = datum)
      ).value

      override def buildTransferAmountTransaction(
        transferType:      ValueTypeIdentifier,
        txos:              Seq[Txo],
        lockPredicateFrom: Lock.Predicate,
        amount:            Long,
        recipientLockAddr: LockAddress,
        changeLockAddr:    LockAddress,
        fee:               Long
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          fromLockAddr <- EitherT.right(lockAddress(Lock().withPredicate(lockPredicateFrom)))
          _ <- EitherT
            .fromEither[F](validateTransferAmountParams(txos, fromLockAddr, amount, transferType, fee))
            .leftMap(errs => UserInputErrors(errs.toList))
          stxoAttestation <- EitherT.right(unprovenAttestation(lockPredicateFrom))
          datum           <- EitherT.right(datum())
          stxos           <- buildStxos(txos, stxoAttestation)
          utxos <- buildUtxos(txos, transferType.some, BigInt(amount).some, recipientLockAddr, changeLockAddr, fee)
        } yield IoTransaction(inputs = stxos, outputs = utxos, datum = datum)
      ).value

      private def buildStxos(
        txos: Seq[Txo],
        att:  Attestation
      ): EitherT[F, BuilderError, Seq[SpentTransactionOutput]] =
        EitherT.rightT(txos.map(x => SpentTransactionOutput(x.outputAddress, att, x.transactionOutput.value)))

      private def buildUtxos(
        txos:             Seq[Txo],
        transferTypeOpt:  Option[ValueTypeIdentifier], // If not provided, then we are transferring all
        amount:           Option[BigInt], // If not provided, then we are transferring all
        recipientAddress: LockAddress,
        changeAddress:    LockAddress,
        fee:              Long
      ): EitherT[F, BuilderError, Seq[UnspentTransactionOutput]] = Try {
        val groupedValues = applyFee(fee, txos.map(_.transactionOutput.value.value).groupBy(_.typeIdentifier))
        val otherVals = (groupedValues -- transferTypeOpt).values.toSeq.flatMap(DefaultAggregationOps.aggregate)
        val (transferVals, changeVals) = transferTypeOpt match {
          // If transferTypeOpt is provided, then we need to calculate what goes to the recipient vs to change
          case Some(transferType) =>
            DefaultAggregationOps
              .aggregateWithChange(groupedValues.getOrElse(transferType, Seq.empty), amount)
              .map(_ ++ otherVals) // otherVals, in addition to the change, goes to the change address
          // If transferTypeOpt is not provided, then all of otherVals goes to the recipient
          case _ => (otherVals, Seq.empty)
        }

        val toRecipient = transferVals
          .map(Value.defaultInstance.withValue)
          .map(UnspentTransactionOutput(recipientAddress, _))
        val toChange = changeVals.map(Value.defaultInstance.withValue).map(UnspentTransactionOutput(changeAddress, _))
        toRecipient ++ toChange
      } match {
        case Success(utxos) => EitherT.rightT(utxos)
        case Failure(err) => EitherT.leftT(BuilderRuntimeError(s"Failed to build utxos. cause: ${err.getMessage}", err))
      }

      /**
       * Apply the fee to the LVL values.
       * Due to validation, we know that there are enough LVLs in the values to satisfy the fee.
       * If there are no LVLs, then we don't need to apply the fee.
       *
       * @param fee The fee to apply to the LVLs
       * @param values The values of the transaction's inputs.
       * @return The values with the LVLs aggregated together and reduced by the fee amount. If there are no LVLs, then
       *         the values are returned unchanged. In this case, we know that the fee is 0.
       */
      private def applyFee(
        fee:    Long,
        values: Map[ValueTypeIdentifier, Seq[BoxValue]]
      ): Map[ValueTypeIdentifier, Seq[BoxValue]] = values.get(LvlType) match {
        case Some(lvlVals) =>
          val newLvlVal = DefaultAggregationOps.aggregateWithChange(lvlVals, BigInt(fee).some)._2
          if (newLvlVal.isEmpty) values - LvlType
          else values + (LvlType -> newLvlVal)
        case _ => values
      }

      override def buildSimpleGroupMintingTransaction(
        txos:              Seq[Txo],
        lockPredicateFrom: Lock.Predicate,
        groupPolicy:                  GroupPolicy,
        quantityToMint:               Long,
        mintedAddress: LockAddress,
        changeAddress:    LockAddress,
        fee: Long
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          registrationLockAddr <- EitherT.right[BuilderError](lockAddress(Lock().withPredicate(lockPredicateFrom)))
          _ <- EitherT
            .fromEither[F](
              validateConstructorMintingParams(
                txos,
                registrationLockAddr,
                groupPolicy.registrationUtxo,
                quantityToMint,
                fee
              )
            )
            .leftMap(errs => UserInputErrors(errs.toList))
          stxoAttestation <- EitherT.right[BuilderError](unprovenAttestation(registrationLock))
          stxos           <- buildStxos(Seq(registrationTxo), stxoAttestation)
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
            .leftMap(errs => UserInputErrors(errs.toList))
          stxoAttestation <- EitherT.right[BuilderError](unprovenAttestation(registrationLock))
          stxos           <- buildStxos(Seq(registrationTxo), stxoAttestation)
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
            .leftMap(errs => UserInputErrors(errs.toList))
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
