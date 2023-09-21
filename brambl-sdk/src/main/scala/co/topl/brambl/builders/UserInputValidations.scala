package co.topl.brambl.builders

import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.{catsSyntaxEitherId, catsSyntaxOptionId, catsSyntaxValidatedIdBinCompat0, toFoldableOps}
import co.topl.brambl.models.box.{AssetMintingStatement, FungibilityType}
import co.topl.brambl.models.{LockAddress, SeriesId, TransactionOutputAddress}
import co.topl.brambl.models.box.Value._
import quivr.models.Int128
import co.topl.brambl.syntax.{
  int128AsBigInt,
  longAsInt128,
  valueToQuantitySyntaxOps,
  valueToTypeIdentifierSyntaxOps,
  AggregateIdentifier,
  AggregateType,
  GroupFungible,
  LvlType,
  SeriesFungible,
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

  def validTransferSupply(
    desiredQuantity: Int128,
    testValues:      Seq[Value]
  ): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(
      testValues.foldLeft(BigInt(0))((acc, q) => acc + q.quantity) >= desiredQuantity,
      (),
      UserInputError(s"All tokens selected to transfer do not have enough funds to transfer")
    )

  def validFee(
    fee:                  Long,
    testValues:           Seq[Value],
    transferRequirements: Long
  ): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(
      testValues.filter(_.isLvl).map(_.quantity: BigInt).sum >= fee + transferRequirements,
      (),
      UserInputError(s"Not enough LVLs in input to satisfy fee")
    )

  // TODO: Finish implementing
  // if agg type is immutable: then
  // hmm undefined case: If 2 inputs with same type identity, immtable q=1, immutable q=2, we want to transfer only 1 of them
  def validTransferQuantityDescriptors(
    aggregateType: AggregateIdentifier,
    testValues:    Seq[Value]
  ): ValidatedNec[UserInputError, Unit] = ??? // {
//    val groupedValues = testValues.groupBy(_.typeIdentifier.aggregateIdentifier)
//    val toTransfer = groupedValues.get(aggregateType).map(_.map(_.quantity).sum).getOrElse(0)
//    Validated.condNec(
//      testValues.filter(_.isLvl).map(_.quantity: BigInt).sum >= fee + transferRequirements,
//      (),
//      UserInputError(s"Not enough LVLs in input to satisfy fee")
//    )
//  }

  // TODO: Remove these
  // The following two validations are temporary until we support all fungibility types
  def fungibilityType(testType: Option[FungibilityType]): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(
      testType.isEmpty || testType.get == FungibilityType.GROUP_AND_SERIES,
      (),
      UserInputError(s"Unsupported fungibility type. We currently only support GROUP_AND_SERIES")
    )

  def identifierFungibilityType(testType: ValueTypeIdentifier): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(
      testType match {
        case GroupFungible(_, _, _) | SeriesFungible(_, _, _) => false
        case _                                                => true
      },
      (),
      UserInputError(s"Unsupported fungibility type. We currently only support GROUP_AND_SERIES")
    )

  def allValidFungibilityTypes(testValues: Seq[Value]): ValidatedNec[UserInputError, Unit] =
    Validated.condNec(
      // Any asset tokens must have valid fungibility
      testValues.forall(v => !v.isAsset || v.asset.get.fungibility == FungibilityType.GROUP_AND_SERIES),
      (),
      UserInputError(s"All asset tokens must have valid fungibility type. We currently only support GROUP_AND_SERIES")
    )

  object TransactionBuilder {

    def validateTransferParams(
      txos:               Seq[Txo],
      fromLockAddr:       LockAddress,
      amount:             Long,
      transferIdentifier: ValueTypeIdentifier,
      fee:                Long
    ): Either[NonEmptyChain[UserInputError], Unit] = Try {
      val allValues = txos.map(_.transactionOutput.value.value)
      val transferValues = allValues.filter(_.typeIdentifier == transferIdentifier)
      Chain(
        positiveQuantity(Some(amount), "quantity to transfer"),
        allInputLocksMatch(txos.map(_.transactionOutput.address), fromLockAddr, "fromLockAddr"),
        validTransferSupply(amount, transferValues),
        identifierFungibilityType(transferIdentifier),
        allValidFungibilityTypes(allValues),
        validFee(fee, allValues, if (transferIdentifier == LvlType) amount else 0)
//        validTransferQuantityDescriptors(transferIdentifier.aggregateIdentifier, transferValues)
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
        ),
        fungibilityType(seriesTxo.transactionOutput.value.value.series.map(_.fungibility))
      ).fold.toEither

  }
}
