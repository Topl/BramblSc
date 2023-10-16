package co.topl.brambl.builders

import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.{catsSyntaxEitherId, catsSyntaxOptionId, catsSyntaxValidatedIdBinCompat0, toFoldableOps}
import co.topl.brambl.models.box.{AssetMintingStatement, FungibilityType, Lock, QuantityDescriptorType}
import co.topl.brambl.models.{LockAddress, SeriesId, TransactionOutputAddress}
import co.topl.brambl.models.box.Value._
import quivr.models.Int128
import co.topl.brambl.syntax.{
  int128AsBigInt,
  longAsInt128,
  valueToQuantitySyntaxOps,
  valueToTypeIdentifierSyntaxOps,
  LvlType,
  ValueTypeIdentifier
}
import co.topl.genus.services.Txo

import scala.util.{Failure, Success, Try}

object UserInputValidations {

  def txosContainsExactlyOneAddress(
    expectedAddr: TransactionOutputAddress,
    expetedLabel: String,
    testTxos:     Seq[Txo]
  ): ValidatedNec[UserInputError, Txo] = {
    val filteredTxos = testTxos.filter(_.outputAddress == expectedAddr)
    val testTxo = Option.when(filteredTxos.length == 1)(filteredTxos.head)
    Validated.condNec(
      testTxo.isDefined,
      testTxo.get,
      UserInputError(s"Input TXOs need to contain exactly one txo matching the $expetedLabel")
    )
  }

  def isLvls(testValue: Value, testLabel: String): ValidatedNec[UserInputError, LVL] =
    Validated.condNec(testValue.isLvl, testValue.lvl.get, UserInputError(s"$testLabel does not contain LVLs"))

  def isGroup(testValue: Value, testLabel: String): ValidatedNec[UserInputError, Group] =
    Validated.condNec(
      testValue.isGroup,
      testValue.group.get,
      UserInputError(s"$testLabel does not contain Group Constructor Tokens")
    )

  def isSeries(testValue: Value, testLabel: String): ValidatedNec[UserInputError, Series] =
    Validated.condNec(
      testValue.isSeries,
      testValue.series.get,
      UserInputError(s"$testLabel does not contain Series Constructor Tokens")
    )

  def fixedSeriesMatch(
    testValue:     Option[SeriesId],
    expectedValue: SeriesId
  ): ValidatedNec[UserInputError, Unit] =
    testValue match {
      case Some(expectedSeries) =>
        Validated.condNec(
          expectedValue == expectedSeries,
          (),
          UserInputError(s"fixedSeries does not match provided Series ID")
        )
      case _ => ().validNec[UserInputError]
    }

  def allInputLocksMatch(
    testAddrs:     Set[LockAddress],
    expectedAddrs: Set[LockAddress],
    testLabel:     String,
    expectedLabel: String
  ): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(
      testAddrs.forall(expectedAddrs.contains),
      (),
      UserInputError(s"every lock in $testLabel must correspond to $expectedLabel")
    )

  def positiveQuantity(testQuantity: Int128, testLabel: String): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(testQuantity > 0, (), UserInputError(s"$testLabel must be positive"))

  def validMintingSupply(
    desiredQuantity: Int128,
    seriesToken:     Series
  ): ValidatedNec[UserInputError, Unit] =
    seriesToken.tokenSupply match {
      case Some(tokenSupply) =>
        if (desiredQuantity % tokenSupply != 0)
          UserInputError(s"quantity to mint must be a multiple of token supply").invalidNec[Unit]
        else if (desiredQuantity > seriesToken.quantity * tokenSupply)
          UserInputError(s"quantity to mint must be less than total token supply available.").invalidNec[Unit]
        else ().validNec[UserInputError]
      case _ => ().validNec[UserInputError]
    }

  def validTransferSupplyAmount(
    desiredQuantity:    Int128,
    allValues:          Seq[Value],
    transferIdentifier: ValueTypeIdentifier
  ): ValidatedNec[UserInputError, Unit] = {
    val testValues = allValues.filter(_.typeIdentifier == transferIdentifier)
    val inputQuantity = testValues.foldLeft(BigInt(0))((acc, q) => acc + q.quantity)
    Validated.condNec(
      inputQuantity >= desiredQuantity,
      (),
      UserInputError(
        s"All tokens selected to transfer do not have enough funds to transfer. " +
        s"The desired quantity to transfer is ${(desiredQuantity: BigInt).intValue} but the ${testValues.length} " +
        s"tokens selected to transfer only have a combined quantity of ${inputQuantity.intValue}."
      )
    )
  }

  def validTransferSupplyAll(
    tokenIdentifier: Option[ValueTypeIdentifier],
    testValues:      Seq[ValueTypeIdentifier]
  ): ValidatedNec[UserInputError, Unit] = tokenIdentifier match {
    case Some(vType) =>
      Validated.condNec(
        testValues.contains(vType),
        (),
        UserInputError(s"When tokenIdentifier is provided, there must be some Txos that match the tokenIdentifier.")
      )
    case None =>
      Validated.condNec(testValues.nonEmpty, (), UserInputError(s"There must be at least one Txo to transfer."))
  }

  def validFee(
    fee:                  Long,
    testValues:           Seq[Value],
    transferRequirements: Long = 0
  ): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(
      testValues.filter(_.isLvl).map(_.quantity: BigInt).sum >= fee + transferRequirements,
      (),
      UserInputError(s"Not enough LVLs in input to satisfy fee")
    )

  def identifierQuantityDescriptorLiquidOrNone(testType: ValueTypeIdentifier): ValidatedNec[UserInputError, Unit] = {
    val qd = testType.getQuantityDescriptor
    Validated.condNec(
      qd.isEmpty || qd.get == QuantityDescriptorType.LIQUID,
      (),
      UserInputError(s"Invalid asset quantity descriptor type. If identifier is an asset, it must be liquid.")
    )
  }

  object TransactionBuilder {

    /**
     * Validates the parameters for transferring all tokens.
     * If user parameters are invalid, return a UserInputError.
     */
    def validateTransferAllParams(
      txos:            Seq[Txo],
      fromLockAddr:    LockAddress,
      fee:             Long,
      tokenIdentifier: Option[ValueTypeIdentifier]
    ): Either[NonEmptyChain[UserInputError], Unit] = Try {
      val allValues = txos.map(_.transactionOutput.value.value)
      Chain(
        allInputLocksMatch(
          txos.map(_.transactionOutput.address).toSet,
          Set(fromLockAddr),
          "the txos",
          "lockPredicateFrom"
        ),
        validTransferSupplyAll(tokenIdentifier, allValues.map(_.typeIdentifier)),
        validFee(fee, allValues)
      ).fold.toEither
    } match {
      case Success(value) => value
      case Failure(err)   => NonEmptyChain.one(UserInputError(err.getMessage)).asLeft
    }

    /**
     * Validates the parameters for transferring a specific amount of a token.
     * If user parameters are invalid, return a UserInputError.
     */
    def validateTransferAmountParams(
      txos:               Seq[Txo],
      fromLockAddr:       LockAddress,
      amount:             Long,
      transferIdentifier: ValueTypeIdentifier,
      fee:                Long
    ): Either[NonEmptyChain[UserInputError], Unit] = Try {
      val allValues = txos.map(_.transactionOutput.value.value)
      Chain(
        positiveQuantity(amount, "quantity to transfer"),
        allInputLocksMatch(
          txos.map(_.transactionOutput.address).toSet,
          Set(fromLockAddr),
          "the txos",
          "lockPredicateFrom"
        ),
        validTransferSupplyAmount(amount, allValues, transferIdentifier),
        identifierQuantityDescriptorLiquidOrNone(transferIdentifier),
        validFee(fee, allValues, if (transferIdentifier == LvlType) amount else 0)
      ).fold.toEither
    } match {
      case Success(value) => value
      case Failure(err)   => NonEmptyChain.one(UserInputError(err.getMessage)).asLeft
    }

    /**
     * Validates the parameters for minting group and series constructor tokens
     * If user parameters are invalid, return a UserInputError.
     */
    def validateConstructorMintingParams(
      txos:                   Seq[Txo],
      fromLockAddr:           LockAddress,
      policyRegistrationUtxo: TransactionOutputAddress,
      quantityToMint:         Long,
      fee:                    Long
    ): Either[NonEmptyChain[UserInputError], Unit] = Try {
      Chain(
        txosContainsExactlyOneAddress(policyRegistrationUtxo, "registrationUtxo", txos)
          .andThen(txo => isLvls(txo.transactionOutput.value.value, "registrationUtxo").map(_ => ())),
        allInputLocksMatch(
          txos.map(_.transactionOutput.address).toSet,
          Set(fromLockAddr),
          "the txos",
          "lockPredicateFrom"
        ),
        positiveQuantity(quantityToMint, "quantityToMint"),
        validFee(fee, txos.map(_.transactionOutput.value.value))
      ).fold.toEither
    } match {
      case Success(value) => value
      case Failure(err)   => NonEmptyChain.one(UserInputError(err.getMessage)).asLeft
    }

    /**
     * Validates the parameters for minting asset tokens
     * If user parameters are invalid, return a UserInputError.
     */
    def validateAssetMintingParams(
      mintingStatement: AssetMintingStatement,
      txos:             Seq[Txo],
      locks:            Set[LockAddress],
      fee:              Long
    ): Either[NonEmptyChain[UserInputError], Unit] = Try {
      val txoLocks = txos.map(_.transactionOutput.address).toSet
      Chain(
        allInputLocksMatch(txoLocks, locks, "the txos", "a lock in the lock map"),
        allInputLocksMatch(locks, txoLocks, "the lock map", "a lock in the txos"),
        (
          txosContainsExactlyOneAddress(mintingStatement.groupTokenUtxo, "registrationUtxo", txos)
            .andThen(txo => isGroup(txo.transactionOutput.value.value, "groupTokenUtxo"))
          product
          txosContainsExactlyOneAddress(mintingStatement.seriesTokenUtxo, "registrationUtxo", txos)
            .andThen(txo => isSeries(txo.transactionOutput.value.value, "seriesTokenUtxo"))
            .andThen(s => positiveQuantity(s.quantity, "quantity of input series constructor tokens").map(_ => s))
            .andThen(s => validMintingSupply(mintingStatement.quantity, s).map(_ => s))
        ).andThen(res => fixedSeriesMatch(res._1.fixedSeries, res._2.seriesId)),
        positiveQuantity(mintingStatement.quantity, "quantity to mint"),
        validFee(fee, txos.map(_.transactionOutput.value.value))
      ).fold.toEither
    } match {
      case Success(value) => value
      case Failure(err)   => NonEmptyChain.one(UserInputError(err.getMessage)).asLeft
    }
  }
}
