package co.topl.brambl.builders

import cats.Id
import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.MockHelpers
import co.topl.brambl.builders.TransactionBuilderInterpreterSpecBase.{
  BuildAssetMintingTransaction,
  BuildGroupConstructorMintingTransaction,
  BuildSeriesConstructorMintingTransaction,
  BuildTransferAllTransaction,
  BuildTransferAmountTransaction
}
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances.{spentOutputImmutable, unspentOutputImmutable}
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.{AssetMintingStatement, Lock, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{LockAddress, TransactionOutputAddress}
import co.topl.brambl.syntax.{
  assetAsBoxVal,
  groupAsBoxVal,
  groupPolicyAsGroupPolicySyntaxOps,
  longAsInt128,
  seriesAsBoxVal,
  seriesPolicyAsSeriesPolicySyntaxOps,
  valueToTypeIdentifierSyntaxOps,
  LvlType,
  ValueTypeIdentifier
}
import co.topl.genus.services.Txo
import co.topl.genus.services.TxoState.UNSPENT

trait TransactionBuilderInterpreterSpecBase extends munit.FunSuite with MockHelpers {
  val txBuilder: TransactionBuilderApi[Id] = TransactionBuilderApi.make[Id](0, 0)

  val RecipientAddr: LockAddress = inLockFullAddress
  val ChangeAddr: LockAddress = trivialLockAddress

  def buildTransferAmountTransaction: BuildTransferAmountTransaction[Id] =
    BuildTransferAmountTransaction(txBuilder, LvlType, mockTxos, inPredicateLockFull, 1, RecipientAddr, ChangeAddr, 1)

  def buildTransferAllTransaction: BuildTransferAllTransaction[Id] =
    BuildTransferAllTransaction(txBuilder, mockTxos, inPredicateLockFull, RecipientAddr, ChangeAddr, 1, None)

  def buildMintGroupTransaction: BuildGroupConstructorMintingTransaction[Id] =
    BuildGroupConstructorMintingTransaction(
      txBuilder,
      mockTxos :+ valToTxo(lvlValue, txAddr = mockGroupPolicyAlt.registrationUtxo),
      inPredicateLockFull,
      mockGroupPolicyAlt,
      1,
      RecipientAddr,
      ChangeAddr,
      2
    )

  def buildMintSeriesTransaction: BuildSeriesConstructorMintingTransaction[Id] =
    BuildSeriesConstructorMintingTransaction(
      txBuilder,
      mockTxos :+ valToTxo(lvlValue, txAddr = mockSeriesPolicyAlt.registrationUtxo),
      inPredicateLockFull,
      mockSeriesPolicyAlt,
      1,
      RecipientAddr,
      ChangeAddr,
      2
    )

  def mockAssetMintingStatement: AssetMintingStatement =
    AssetMintingStatement(mockGroupPolicyAlt.registrationUtxo, mockSeriesPolicyAlt.registrationUtxo, 1)

  def buildMintAssetTransaction: BuildAssetMintingTransaction[Id] =
    BuildAssetMintingTransaction(
      txBuilder,
      mockAssetMintingStatement,
      mockTxos :+ valToTxo(groupValue, txAddr = mockGroupPolicyAlt.registrationUtxo) :+ valToTxo(
        seriesValue,
        txAddr = mockSeriesPolicyAlt.registrationUtxo
      ),
      Map(inLockFullAddress -> inPredicateLockFull),
      1,
      RecipientAddr,
      ChangeAddr
    )

  def valToTxo(
    value:    Value,
    lockAddr: LockAddress = inLockFullAddress,
    txAddr:   TransactionOutputAddress = dummyTxoAddress
  ): Txo =
    Txo(valToUtxo(value, lockAddr), UNSPENT, txAddr)

  def valToUtxo(value: Value, lockAddr: LockAddress = inLockFullAddress): UnspentTransactionOutput =
    UnspentTransactionOutput(lockAddr, value)

  def sortedTx(tx: IoTransaction): IoTransaction = tx
    .withOutputs(tx.outputs.sortBy(_.immutable.value.toByteArray.mkString))
    .withInputs(tx.inputs.sortBy(_.immutable.value.toByteArray.mkString))

  def toAltAsset(asset: Value): Value = asset.copy(
    asset.getAsset.copy(
      groupId = mockGroupPolicyAlt.computeId.some,
      seriesId = mockSeriesPolicyAlt.computeId.some
    )
  )

  def buildStxos(txos: Seq[Txo] = mockTxos): Seq[SpentTransactionOutput] =
    txos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value))

  def buildUtxos(values: Seq[Value], lockAddr: LockAddress): Seq[UnspentTransactionOutput] =
    values.map(valToUtxo(_, lockAddr))

  def buildRecipientUtxos(values: Seq[Value]): Seq[UnspentTransactionOutput] = buildUtxos(values, RecipientAddr)
  def buildChangeUtxos(values:    Seq[Value]): Seq[UnspentTransactionOutput] = buildUtxos(values, ChangeAddr)

  val mockSeriesPolicyAlt: SeriesPolicy = SeriesPolicy("Mock Series Policy", None, dummyTxoAddress.copy(index = 44))
  val mockGroupPolicyAlt: GroupPolicy = GroupPolicy("Mock Group Policy", dummyTxoAddress.copy(index = 55))

  val groupValueAlt: Value = groupValue.copy(groupValue.getGroup.copy(groupId = mockGroupPolicyAlt.computeId))
  val seriesValueAlt: Value = seriesValue.copy(seriesValue.getSeries.copy(seriesId = mockSeriesPolicyAlt.computeId))

  val assetGroupSeriesAlt: Value = toAltAsset(assetGroupSeries)
  val assetGroupAlt: Value = toAltAsset(assetGroup)
  val assetSeriesAlt: Value = toAltAsset(assetSeries)

  val assetGroupSeriesAccumulatorAlt: Value = toAltAsset(assetGroupSeriesAccumulator)
  val assetGroupAccumulatorAlt: Value = toAltAsset(assetGroupAccumulator)
  val assetSeriesAccumulatorAlt: Value = toAltAsset(assetSeriesAccumulator)

  val assetGroupSeriesFractionableAlt: Value = toAltAsset(assetGroupSeriesFractionable)
  val assetGroupFractionableAlt: Value = toAltAsset(assetGroupFractionable)
  val assetSeriesFractionableAlt: Value = toAltAsset(assetSeriesFractionable)

  val assetGroupSeriesImmutableAlt: Value = toAltAsset(assetGroupSeriesImmutable)
  val assetGroupImmutableAlt: Value = toAltAsset(assetGroupImmutable)
  val assetSeriesImmutableAlt: Value = toAltAsset(assetSeriesImmutable)

  val mockValues: Seq[Value] = Seq(
    lvlValue,
    lvlValue.copy(), // exact duplicate
    groupValue,
    groupValue.copy(), // exact duplicate
    groupValueAlt, // diff group
    seriesValue,
    seriesValue.copy(), // exact duplicate
    seriesValueAlt, // diff series
    assetGroupSeries,
    assetGroupSeries.copy(), // exact duplicate
    assetGroupSeriesAlt, // diff group and series
    assetGroup,
    assetGroup.copy(),
    assetGroupAlt, // diff group and series
    assetSeries,
    assetSeries.copy(),
    assetSeriesAlt, // diff group and series
    assetGroupSeriesAccumulator,
    assetGroupSeriesAccumulator.copy(),
    assetGroupSeriesAccumulatorAlt, // diff group and series
    assetGroupAccumulator,
    assetGroupAccumulator.copy(),
    assetGroupAccumulatorAlt, // diff group and series
    assetSeriesAccumulator,
    assetSeriesAccumulator.copy(),
    assetSeriesAccumulatorAlt, // diff group and series
    assetGroupSeriesFractionable,
    assetGroupSeriesFractionable.copy(),
    assetGroupSeriesFractionableAlt, // diff group and series
    assetGroupFractionable,
    assetGroupFractionable.copy(),
    assetGroupFractionableAlt, // diff group and series
    assetSeriesFractionable,
    assetSeriesFractionable.copy(),
    assetSeriesFractionableAlt, // diff group and series
    assetGroupSeriesImmutable,
    assetGroupSeriesImmutable.copy(),
    assetGroupSeriesImmutableAlt, // diff group and series
    assetGroupImmutable,
    assetGroupImmutable.copy(),
    assetGroupImmutableAlt, // diff group and series
    assetSeriesImmutable,
    assetSeriesImmutable.copy(),
    assetSeriesImmutableAlt // diff group and series
  )

  val mockTxos: Seq[Txo] = mockValues.map(valToTxo(_))

  def mockChange: Seq[Value] = mockChange(mockTxos)

  def mockChange(txos: Seq[Txo]): Seq[Value] = txos
    .map(_.transactionOutput.value)
    .map(_.value)
    .groupBy(_.typeIdentifier)
    .view
    .mapValues(DefaultAggregationOps.aggregate)
    .values
    .flatten
    .toSeq
    .map(Value.defaultInstance.withValue)

}

/**
 * Helpers for the TransactionBuilder test cases
 */
object TransactionBuilderInterpreterSpecBase {

  case class BuildGroupConstructorMintingTransaction[F[_]](
    txBuilder:         TransactionBuilderApi[F],
    txos:              Seq[Txo],
    lockPredicateFrom: Lock.Predicate,
    groupPolicy:       GroupPolicy,
    quantityToMint:    Long,
    mintedAddress:     LockAddress,
    changeAddress:     LockAddress,
    fee:               Long
  ) {

    def addTxo(txo: Txo): BuildGroupConstructorMintingTransaction[F] = this.copy(txos = txos :+ txo)

    def withPolicy(groupPolicy: GroupPolicy): BuildGroupConstructorMintingTransaction[F] =
      this.copy(groupPolicy = groupPolicy)

    def withMintAmount(quantityToMint: Long): BuildGroupConstructorMintingTransaction[F] =
      this.copy(quantityToMint = quantityToMint)

    def withFee(fee: Long): BuildGroupConstructorMintingTransaction[F] = this.copy(fee = fee)

    def run: F[Either[BuilderError, IoTransaction]] = txBuilder
      .buildGroupMintingTransaction(
        txos,
        lockPredicateFrom,
        groupPolicy,
        quantityToMint,
        mintedAddress,
        changeAddress,
        fee
      )
  }

  case class BuildSeriesConstructorMintingTransaction[F[_]](
    txBuilder:         TransactionBuilderApi[F],
    txos:              Seq[Txo],
    lockPredicateFrom: Lock.Predicate,
    seriesPolicy:      SeriesPolicy,
    quantityToMint:    Long,
    mintedAddress:     LockAddress,
    changeAddress:     LockAddress,
    fee:               Long
  ) {

    def addTxo(txo: Txo): BuildSeriesConstructorMintingTransaction[F] = this.copy(txos = txos :+ txo)

    def withPolicy(seriesPolicy: SeriesPolicy): BuildSeriesConstructorMintingTransaction[F] =
      this.copy(seriesPolicy = seriesPolicy)

    def withMintAmount(quantityToMint: Long): BuildSeriesConstructorMintingTransaction[F] =
      this.copy(quantityToMint = quantityToMint)

    def withFee(fee: Long): BuildSeriesConstructorMintingTransaction[F] = this.copy(fee = fee)

    def run: F[Either[BuilderError, IoTransaction]] = txBuilder
      .buildSeriesMintingTransaction(
        txos,
        lockPredicateFrom,
        seriesPolicy,
        quantityToMint,
        mintedAddress,
        changeAddress,
        fee
      )
  }

  case class BuildAssetMintingTransaction[F[_]](
    txBuilder:              TransactionBuilderApi[F],
    mintingStatement:       AssetMintingStatement,
    txos:                   Seq[Txo],
    locks:                  Map[LockAddress, Lock.Predicate],
    fee:                    Long,
    mintedAssetLockAddress: LockAddress,
    changeAddress:          LockAddress
  ) {

    def addTxo(txo: Txo): BuildAssetMintingTransaction[F] = this.copy(txos = txos :+ txo)

    def addLock(lock: (LockAddress, Lock.Predicate)): BuildAssetMintingTransaction[F] =
      this.copy(locks = locks + lock)

    def updateAmsQuantity(quantityToMint: Long): BuildAssetMintingTransaction[F] =
      this.copy(mintingStatement = mintingStatement.copy(quantity = quantityToMint))

    def updateAmsGroupUtxo(groupUtxo: TransactionOutputAddress): BuildAssetMintingTransaction[F] =
      this.copy(mintingStatement = mintingStatement.copy(groupTokenUtxo = groupUtxo))

    def updateAmsSeriesUtxo(seriesUtxo: TransactionOutputAddress): BuildAssetMintingTransaction[F] =
      this.copy(mintingStatement = mintingStatement.copy(seriesTokenUtxo = seriesUtxo))

    def withFee(fee: Long): BuildAssetMintingTransaction[F] = this.copy(fee = fee)

    def run: F[Either[BuilderError, IoTransaction]] = txBuilder
      .buildSimpleAssetMintingTransaction(
        mintingStatement,
        txos,
        locks,
        fee,
        mintedAssetLockAddress,
        changeAddress,
        None,
        None
      )
  }

  case class BuildTransferAllTransaction[F[_]](
    txBuilder:         TransactionBuilderApi[F],
    txos:              Seq[Txo],
    lockPredicateFrom: Lock.Predicate,
    recipientLockAddr: LockAddress,
    changeLockAddr:    LockAddress,
    fee:               Long,
    tokenIdentifier:   Option[ValueTypeIdentifier]
  ) {

    def withTokenIdentifier(tokenIdentifier: ValueTypeIdentifier): BuildTransferAllTransaction[F] =
      this.copy(tokenIdentifier = tokenIdentifier.some)

    def noTokenIdentifier: BuildTransferAllTransaction[F] =
      this.copy(tokenIdentifier = None)

    def withTxos(txos: Seq[Txo]): BuildTransferAllTransaction[F] = this.copy(txos = txos)

    def withFromPredicate(lockPredicateFrom: Lock.Predicate): BuildTransferAllTransaction[F] =
      this.copy(lockPredicateFrom = lockPredicateFrom)

    def withRecipientLockAddr(recipientLockAddr: LockAddress): BuildTransferAllTransaction[F] =
      this.copy(recipientLockAddr = recipientLockAddr)

    def withChangeLockAddr(changeLockAddr: LockAddress): BuildTransferAllTransaction[F] =
      this.copy(changeLockAddr = changeLockAddr)

    def withFee(fee: Long): BuildTransferAllTransaction[F] = this.copy(fee = fee)

    def run: F[Either[BuilderError, IoTransaction]] =
      txBuilder.buildTransferAllTransaction(
        txos,
        lockPredicateFrom,
        recipientLockAddr,
        changeLockAddr,
        fee,
        tokenIdentifier
      )
  }

  case class BuildTransferAmountTransaction[F[_]](
    txBuilder:         TransactionBuilderApi[F],
    tokenIdentifier:   ValueTypeIdentifier,
    txos:              Seq[Txo],
    lockPredicateFrom: Lock.Predicate,
    amount:            Long,
    recipientLockAddr: LockAddress,
    changeLockAddr:    LockAddress,
    fee:               Long
  ) {

    def withTokenIdentifier(tokenIdentifier: ValueTypeIdentifier): BuildTransferAmountTransaction[F] =
      this.copy(tokenIdentifier = tokenIdentifier)

    def withTxos(txos: Seq[Txo]): BuildTransferAmountTransaction[F] = this.copy(txos = txos)

    def withFromPredicate(lockPredicateFrom: Lock.Predicate): BuildTransferAmountTransaction[F] =
      this.copy(lockPredicateFrom = lockPredicateFrom)

    def withAmount(amount: Long): BuildTransferAmountTransaction[F] = this.copy(amount = amount)

    def withRecipientLockAddr(recipientLockAddr: LockAddress): BuildTransferAmountTransaction[F] =
      this.copy(recipientLockAddr = recipientLockAddr)

    def withChangeLockAddr(changeLockAddr: LockAddress): BuildTransferAmountTransaction[F] =
      this.copy(changeLockAddr = changeLockAddr)

    def withFee(fee: Long): BuildTransferAmountTransaction[F] = this.copy(fee = fee)

    def run: F[Either[BuilderError, IoTransaction]] = txBuilder
      .buildTransferAmountTransaction(
        tokenIdentifier,
        txos,
        lockPredicateFrom,
        amount,
        recipientLockAddr,
        changeLockAddr,
        fee
      )
  }
}
