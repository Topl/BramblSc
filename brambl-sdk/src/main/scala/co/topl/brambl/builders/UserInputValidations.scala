package co.topl.brambl.builders

import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.{catsSyntaxEitherId, catsSyntaxOptionId, catsSyntaxValidatedIdBinCompat0, toFoldableOps}
import co.topl.brambl.models.box.{AssetMintingStatement, FungibilityType, QuantityDescriptorType}
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

  def txoAddressMatch(
    testAddr:      TransactionOutputAddress,
    expectedAddr:  TransactionOutputAddress,
    testLabel:     String,
    expectedLabel: String
  ): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(testAddr == expectedAddr, (), UserInputError(s"$testLabel does not match $expectedLabel"))

  def txosContainsAddress(
    expectedAddr:  TransactionOutputAddress,
    expetedLabel: String,
    testTxos: Seq[Txo]
  ): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(testTxos.count(_.outputAddress == expectedAddr) == 1, (), UserInputError(s"Input TXOs need to contain exactly one txo matching the $expetedLabel"))

  def isLvls(testValue: Value, testLabel: String): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(testValue.isLvl, (), UserInputError(s"$testLabel does not contain LVLs"))

  def isGroup(testValue: Value, testLabel: String): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(testValue.isGroup, (), UserInputError(s"$testLabel does not contain Group Constructor Tokens"))

  def isSeries(testValue: Value, testLabel: String): ValidatedNec[UserInputError, Unit] =
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

  def allInputLocksMatch(
    testAddrs:     Seq[LockAddress],
    expectedAddr:  LockAddress,
    expectedLabel: String
  ): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(
      testAddrs.forall(_ == expectedAddr),
      (),
      UserInputError(s"every lock does not correspond to $expectedLabel")
    )

  def positiveQuantity(testQuantity: Option[Int128], testLabel: String): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(
      testQuantity.isEmpty || testQuantity.get > 0,
      (),
      UserInputError(s"$testLabel must be positive")
    )

  def validMintingSupply(
    desiredQuantity: Int128,
    seriesTokenOpt:  Option[Series],
    testLabel:       String
  ): ValidatedNec[UserInputError, Unit] =
    seriesTokenOpt match {
      case Some(Series(_, availableQuantity, Some(tokenSupply), _, _, _)) =>
        if (desiredQuantity % tokenSupply != 0)
          UserInputError(s"$testLabel must be a multiple of token supply").invalidNec[Unit]
        else if (desiredQuantity > availableQuantity * tokenSupply)
          UserInputError(s"$testLabel must be less than total token supply available.").invalidNec[Unit]
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

    def validateTransferAllParams(
      txos:            Seq[Txo],
      fromLockAddr:    LockAddress,
      fee:             Long,
      tokenIdentifier: Option[ValueTypeIdentifier]
    ): Either[NonEmptyChain[UserInputError], Unit] = Try {
      val allValues = txos.map(_.transactionOutput.value.value)
      Chain(
        allInputLocksMatch(txos.map(_.transactionOutput.address), fromLockAddr, "fromLockAddr"),
        validTransferSupplyAll(tokenIdentifier, allValues.map(_.typeIdentifier)),
        validFee(fee, allValues)
      ).fold.toEither
    } match {
      case Success(value) => value
      case Failure(err)   => NonEmptyChain.one(UserInputError(err.getMessage)).asLeft
    }

    def validateTransferAmountParams(
      txos:               Seq[Txo],
      fromLockAddr:       LockAddress,
      amount:             Long,
      transferIdentifier: ValueTypeIdentifier,
      fee:                Long
    ): Either[NonEmptyChain[UserInputError], Unit] = Try {
      val allValues = txos.map(_.transactionOutput.value.value)
      Chain(
        positiveQuantity(Some(amount), "quantity to transfer"),
        allInputLocksMatch(txos.map(_.transactionOutput.address), fromLockAddr, "fromLockAddr"),
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
      txos:        Seq[Txo],
      fromLockAddr:   LockAddress,
      policyRegistrationUtxo: TransactionOutputAddress,
      quantityToMint:         Long,
      fee:                   Long
    ): Either[NonEmptyChain[UserInputError], Unit] =
      Chain(
        txosContainsAddress(policyRegistrationUtxo, "registrationUtxo", txos),
        isLvls(registrationTxo.transactionOutput.value.value, "registrationUtxo"),
        allInputLocksMatch(txos.map(_.transactionOutput.address), fromLockAddr, "fromLockAddr"),
        positiveQuantity(Some(quantityToMint), "quantityToMint"),
        validFee(fee, txos.map(_.transactionOutput.value.value))
      ).fold.toEither

    /**
     * Validates the parameters for minting asset tokens
     * If user parameters are invalid, return a UserInputError.
     */
    def validateAssetMintingParams(
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
        )
      ).fold.toEither

  }
}
