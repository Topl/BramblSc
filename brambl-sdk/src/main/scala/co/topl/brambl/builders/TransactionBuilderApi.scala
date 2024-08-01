package co.topl.brambl.builders

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import co.topl.brambl.builders.UserInputValidations.TransactionBuilder._
import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.Value.{Value => BoxValue}
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models._
import co.topl.brambl.syntax.{LvlType, UnknownType, ValueTypeIdentifier, bigIntAsInt128, groupPolicyAsGroupPolicySyntaxOps, int128AsBigInt, longAsInt128, seriesPolicyAsSeriesPolicySyntaxOps, valueToQuantitySyntaxOps, valueToTypeIdentifierSyntaxOps}
import co.topl.genus.services.Txo
import com.google.protobuf.ByteString
import com.google.protobuf.struct.Struct
import quivr.models.{Int128, Proof, SmallData}

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
 * Defines a builder for IoTransaction
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
   * @param txos All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain some token
   *             matching tokenIdentifier (if it is provided) and at least the quantity of LVLs to satisfy the fee. Else
   *             an error will be returned. Any TXOs that contain values of an invalid type, such as UnknownType, will be
   *             filtered out and won't be included in the inputs.
   * @param lockPredicateFrom The Lock Predicate encumbering the txos
   * @param recipientLockAddress The LockAddress of the recipient
   * @param changeLockAddress A LockAddress to send the tokens that are not going to the recipient
   * @param fee The fee to pay for the transaction. The txos must contain enough LVLs to satisfy this fee
   * @param tokenIdentifier An optional token identifier to denote the type of token to transfer to the recipient. If
   *                        None, all tokens in txos will be transferred to the recipient and changeLockAddress will be
   *                        ignored. This must not be UnknownType.
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
   * @note This function only supports transferring a specific amount of assets (via tokenIdentifier) if their quantity
   *       descriptor type is LIQUID.
   * @note This function only support transferring a specific amount of TOPLs (via tokenIdentifier) if their staking
   *       registration is None.
   * @param tokenIdentifier The Token Identifier denoting the type of token to transfer to the recipient. If this denotes
   *                        an Asset Token, the referenced asset's quantity descriptor type must be LIQUID, else an error
   *                        will be returned. This must not be UnknownType.
   * @param txos All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain at least the
   *             necessary quantity (given by amount) of the identified Token and at least the quantity of LVLs to
   *             satisfy the fee. Else an error will be returned. Any TXOs that contain values of an invalid type, such
   *             as UnknownType, will be filtered out and won't be included in the inputs.
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
   * If successful, the transaction will have one or more inputs (at least the registrationUtxo) and one or more
   * outputs (at least the minted group constructor tokens). There can be more inputs and outputs if the supplied txos
   * contain more tokens.
   *
   * @param txos All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain some LVLs (as
   *             specified in the policy), to satisfy the registration fee. Else an error will be returned. Any TXOs
   *             that contain values of an invalid type, such as UnknownType, will be filtered out and won't be included
   *             in the inputs.
   * @param lockPredicateFrom The Predicate Lock that encumbers the funds in the txos. This will be used in
   *                         the attestations of the inputs.
   * @param groupPolicy The group policy for which we are minting constructor tokens. This group policy specifies a
   *                    registrationUtxo to be used as an input in this transaction.
   * @param quantityToMint The quantity of constructor tokens to mint
   * @param mintedAddress The LockAddress to send the minted constructor tokens to.
   * @param changeAddress The LockAddress to send the change to.
   * @param fee              The transaction fee. The txos must contain enough LVLs to satisfy this fee
   * @return An unproven Group Constructor minting transaction if possible. Else, an error
   */
  def buildGroupMintingTransaction(
    txos:              Seq[Txo],
    lockPredicateFrom: Lock.Predicate,
    groupPolicy:       GroupPolicy,
    quantityToMint:    Long,
    mintedAddress:     LockAddress,
    changeAddress:     LockAddress,
    fee:               Long
  ): F[Either[BuilderError, IoTransaction]]

  /**
   * Builds a simple transaction to mint Series Constructor tokens.
   * If successful, the transaction will have one or more inputs (at least the registrationUtxo) and one or more
   * outputs (at least the minted series constructor tokens). There can be more inputs and outputs if the supplied txos
   * contain more tokens.
   *
   * @param txos              All the TXOs encumbered by the Lock given by lockPredicateFrom. These TXOs must contain
   *                          some LVLs (as specified in the policy), to satisfy the registration fee. Else an error will
   *                          be returned. Any TXOs that contain values of an invalid type, such as UnknownType, will be
   *                          filtered out and won't be included in the inputs.
   * @param lockPredicateFrom The Predicate Lock that encumbers the funds in the txos. This will be used in
   *                          the attestations of the inputs.
   * @param seriesPolicy The series policy for which we are minting constructor tokens. This series policy specifies a
   *                    registrationUtxo to be used as an input in this transaction.
   * @param quantityToMint The quantity of constructor tokens to mint
   * @param mintedAddress The LockAddress to send the minted constructor tokens to.
   * @param changeAddress The LockAddress to send the change to.
   * @param fee           The transaction fee. The txos must contain enough LVLs to satisfy this fee
   * @return An unproven Series Constructor minting transaction if possible. Else, an error
   */
  def buildSeriesMintingTransaction(
    txos:              Seq[Txo],
    lockPredicateFrom: Lock.Predicate,
    seriesPolicy:      SeriesPolicy,
    quantityToMint:    Long,
    mintedAddress:     LockAddress,
    changeAddress:     LockAddress,
    fee:               Long
  ): F[Either[BuilderError, IoTransaction]]

  /**
   * Builds a simple transaction to mint asset tokens.
   * If successful, the transaction will have two or more inputs (at least the group and series registration tokens) and
   * two or more outputs (at least the minted asset tokens and the input group constructor token). There can be more
   * inputs and outputs if the supplied txos contain more tokens.
   *
   * @note If the "tokenSupply" field in the registration series constructor tokens is present, then the quantity of
   *       asset tokens to mint (defined in the AMS) has to be a multiple of this field, else an error will be returned.
   *       In this case, minting each multiple of "tokenSupply" quantity of assets will burn a single series constructor token.
   * @param mintingStatement      The minting statement that specifies the asset to mint.
   * @param txos                  All the TXOs encumbered by the Locks given by locks. These TXOs must contain some
   *                              group and series constructors (as referenced in the AMS) to satisfy the minting
   *                              requirements. Else an error will be returned. Any TXOs that contain values of an invalid
   *                              type, such as UnknownType, will be filtered out and won't be included in the inputs.
   * @param locks             A mapping of Predicate Locks that encumbers the funds in the txos. This will be used in the
   *                              attestations of the txos' inputs.
   * @param fee The transaction fee. The txos must contain enough LVLs to satisfy this fee
   * @param mintedAssetLockAddress The LockAddress to send the minted asset tokens to.
   * @param changeAddress The LockAddress to send the change to.
   * @param ephemeralMetadata     Optional ephemeral metadata to include in the minted asset tokens.
   * @param commitment            Optional commitment to include in the minted asset tokens.
   * @return An unproven asset minting transaction if possible. Else, an error
   */
  def buildAssetMintingTransaction(
    mintingStatement:       AssetMintingStatement,
    txos:                   Seq[Txo],
    locks:                  Map[LockAddress, Lock.Predicate],
    fee:                    Long,
    mintedAssetLockAddress: LockAddress,
    changeAddress:          LockAddress,
    ephemeralMetadata:      Option[Struct] = None,
    commitment:             Option[ByteString] = None
  ): F[Either[BuilderError, IoTransaction]]

  def buildAssetMergeTransaction(
    mergingStatement: AssetMergingStatement,
    txos: Seq[Txo],
    locks: Map[LockAddress, Lock.Predicate],
    fee: Long,
    mergedAssetLockAddress: LockAddress,
    changeAddress: LockAddress,
    ephemeralMetadata: Option[Struct] = None,
    commitment: Option[ByteString] = None
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
          filteredTxos = txos.filter(_.transactionOutput.value.value.typeIdentifier != UnknownType)
          _ <- EitherT
            .fromEither[F](validateTransferAllParams(filteredTxos, fromLockAddr, fee, tokenIdentifier))
            .leftMap(errs => UserInputErrors(errs.toList))
          stxoAttestation <- EitherT.right(unprovenAttestation(lockPredicateFrom))
          datum           <- EitherT.right(datum())
          stxos           <- buildStxos(filteredTxos, stxoAttestation)
          utxos           <- buildUtxos(filteredTxos, tokenIdentifier, None, recipientLockAddr, changeLockAddr, fee)
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
          filteredTxos = txos.filter(_.transactionOutput.value.value.typeIdentifier != UnknownType)
          _ <- EitherT
            .fromEither[F](validateTransferAmountParams(filteredTxos, fromLockAddr, amount, transferType, fee))
            .leftMap(errs => UserInputErrors(errs.toList))
          stxoAttestation <- EitherT.right(unprovenAttestation(lockPredicateFrom))
          datum           <- EitherT.right(datum())
          stxos           <- buildStxos(filteredTxos, stxoAttestation)
          utxos <- buildUtxos(
            filteredTxos,
            transferType.some,
            BigInt(amount).some,
            recipientLockAddr,
            changeLockAddr,
            fee
          )
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

      override def buildGroupMintingTransaction(
        txos:              Seq[Txo],
        lockPredicateFrom: Lock.Predicate,
        groupPolicy:       GroupPolicy,
        quantityToMint:    Long,
        mintedAddress:     LockAddress,
        changeAddress:     LockAddress,
        fee:               Long
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          registrationLockAddr <- EitherT.right[BuilderError](lockAddress(Lock().withPredicate(lockPredicateFrom)))
          filteredTxos = txos.filter(_.transactionOutput.value.value.typeIdentifier != UnknownType)
          _ <- EitherT
            .fromEither[F](
              validateConstructorMintingParams(
                filteredTxos,
                registrationLockAddr,
                groupPolicy.registrationUtxo,
                quantityToMint,
                fee
              )
            )
            .leftMap(errs => UserInputErrors(errs.toList))
          stxoAttestation <- EitherT.right[BuilderError](unprovenAttestation(lockPredicateFrom))
          stxos           <- buildStxos(filteredTxos, stxoAttestation)
          datum           <- EitherT.right[BuilderError](datum())
          utxoMinted <- EitherT.right[BuilderError](
            groupOutput(mintedAddress, quantityToMint, groupPolicy.computeId, groupPolicy.fixedSeries)
          )
          utxoChange <- buildUtxos(filteredTxos, None, None, changeAddress, changeAddress, fee)
        } yield IoTransaction(
          inputs = stxos,
          outputs = utxoChange :+ utxoMinted,
          datum = datum,
          groupPolicies = Seq(Datum.GroupPolicy(groupPolicy))
        )
      ).value

      override def buildSeriesMintingTransaction(
        txos:              Seq[Txo],
        lockPredicateFrom: Lock.Predicate,
        seriesPolicy:      SeriesPolicy,
        quantityToMint:    Long,
        mintedAddress:     LockAddress,
        changeAddress:     LockAddress,
        fee:               Long
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          registrationLockAddr <- EitherT.right[BuilderError](lockAddress(Lock().withPredicate(lockPredicateFrom)))
          filteredTxos = txos.filter(_.transactionOutput.value.value.typeIdentifier != UnknownType)
          _ <- EitherT
            .fromEither[F](
              validateConstructorMintingParams(
                filteredTxos,
                registrationLockAddr,
                seriesPolicy.registrationUtxo,
                quantityToMint,
                fee
              )
            )
            .leftMap(errs => UserInputErrors(errs.toList))
          stxoAttestation <- EitherT.right[BuilderError](unprovenAttestation(lockPredicateFrom))
          stxos           <- buildStxos(filteredTxos, stxoAttestation)
          datum           <- EitherT.right[BuilderError](datum())
          utxoMinted <- EitherT.right[BuilderError](
            seriesOutput(
              mintedAddress,
              quantityToMint,
              seriesPolicy.computeId,
              seriesPolicy.tokenSupply,
              seriesPolicy.fungibility,
              seriesPolicy.quantityDescriptor
            )
          )
          utxoChange <- buildUtxos(filteredTxos, None, None, changeAddress, changeAddress, fee)
        } yield IoTransaction(
          inputs = stxos,
          outputs = utxoChange :+ utxoMinted,
          datum = datum,
          seriesPolicies = Seq(Datum.SeriesPolicy(seriesPolicy))
        )
      ).value

      private def toAttestationMap(
        txos:  Seq[Txo],
        locks: Map[LockAddress, Lock.Predicate]
      ): EitherT[F, BuilderError, Map[Seq[Txo], Attestation]] = {
        val txoMap = txos.groupBy(_.transactionOutput.address)
        // Per validation, we know that all txos have a lock within locks and all locks have a txo corresponding to them
        EitherT.right[BuilderError](
          locks.toSeq.map(el => (txoMap(el._1), unprovenAttestation(el._2)).sequence).sequence.map(_.toMap)
        )
      }

      override def buildAssetMintingTransaction(
        mintingStatement:       AssetMintingStatement,
        txos:                   Seq[Txo],
        locks:                  Map[LockAddress, Lock.Predicate],
        fee:                    Long,
        mintedAssetLockAddress: LockAddress,
        changeAddress:          LockAddress,
        ephemeralMetadata:      Option[Struct] = None,
        commitment:             Option[ByteString] = None
      ): F[Either[BuilderError, IoTransaction]] = (
        for {
          datum <- EitherT.right[BuilderError](datum())
          filteredTxos = txos.filter(_.transactionOutput.value.value.typeIdentifier != UnknownType)
          _ <- EitherT
            .fromEither[F](validateAssetMintingParams(mintingStatement, filteredTxos, locks.keySet, fee))
            .leftMap(errs => UserInputErrors(errs.toList))
          attestations <- toAttestationMap(filteredTxos, locks)
          stxos        <- attestations.map(el => buildStxos(el._1, el._2)).toSeq.sequence.map(_.flatten)
          // Per validation, there is exactly one series token in txos
          (seriesTxo, nonSeriesTxo) = filteredTxos
            .partition(_.outputAddress == mintingStatement.seriesTokenUtxo)
            .leftMap(_.head)
          seriesUtxo = seriesTxo.transactionOutput
          seriesToken = seriesUtxo.value.getSeries
          // Per validation, there is exactly one group token in txos
          groupToken = filteredTxos
            .filter(_.outputAddress == mintingStatement.groupTokenUtxo)
            .head
            .transactionOutput
            .value
            .getGroup
          utxoMinted <- EitherT.right[BuilderError](
            assetOutput(
              mintedAssetLockAddress,
              mintingStatement.quantity,
              groupToken.groupId,
              seriesToken.seriesId,
              seriesToken.fungibility,
              seriesToken.quantityDescriptor,
              ephemeralMetadata,
              commitment
            )
          )
          seriesTxoAdjusted = {
            val inputQuantity = seriesToken.quantity
            val outputQuantity: BigInt =
              if (seriesToken.tokenSupply.isEmpty) inputQuantity
              else inputQuantity - (mintingStatement.quantity / seriesToken.tokenSupply.get)
            if (outputQuantity > 0)
              Seq(
                seriesTxo
                  .withTransactionOutput(
                    seriesUtxo
                      .withValue(
                        Value.defaultInstance
                          .withSeries(seriesToken.withQuantity(outputQuantity))
                      )
                  ) // Only the quantity changes
              )
            else Seq.empty[Txo]
          }
          changeOutputs <- buildUtxos(nonSeriesTxo ++ seriesTxoAdjusted, None, None, changeAddress, changeAddress, fee)
        } yield IoTransaction(
          inputs = stxos,
          outputs = changeOutputs :+ utxoMinted,
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

      override def buildAssetMergeTransaction(mergingStatement: AssetMergingStatement, txos: Seq[Txo], locks: Map[LockAddress, Lock.Predicate], fee: Long, mergedAssetLockAddress: LockAddress, changeAddress: LockAddress, ephemeralMetadata: Option[Struct], commitment: Option[ByteString]): F[Either[BuilderError, IoTransaction]] = {
        // validate arguments
          // verify all the input utxos are present in the txos
          // verify the other stuff (same as the other functions)
        // separate the TXOs. the ones to be merged, vs the ones to go to change
        // validate that the txos to be merged are all compatible
        // create a single merged utxo for the ones to be merged. (MergingOps)
          // merging ops takes all the txos to be merged together, and either throws a validation error or returns a single utxo
        // merging ops will verify that they are all compatible
        // use applyFee for the ones to go to change, and create the utxos for that
        // in the unit tests, test diff cases (its own test suite file, try different utxo combinations (not compatible)).
        ???
      }
    }
}
