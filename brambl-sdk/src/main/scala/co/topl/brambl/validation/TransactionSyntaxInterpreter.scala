package co.topl.brambl.validation

import cats.Applicative
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.implicits._
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.models.{SeriesId, TransactionOutputAddress}
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax._
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import quivr.models.{Int128, Proof, Proposition}
import scala.util.Try

object TransactionSyntaxInterpreter {

  final val MaxDataLength = 15360

  def make[F[_]: Applicative](): TransactionSyntaxVerifier[F] = new TransactionSyntaxVerifier[F] {

    override def validate(t: IoTransaction): F[Either[NonEmptyChain[TransactionSyntaxError], IoTransaction]] =
      validators
        .foldMap(_ apply t)
        .toEither
        .as(t)
        .pure[F]
  }

  private val validators: Chain[IoTransaction => ValidatedNec[TransactionSyntaxError, Unit]] =
    Chain(
      nonEmptyInputsValidation,
      distinctInputsValidation,
      maximumOutputsCountValidation,
      nonNegativeTimestampValidation,
      scheduleValidation,
      positiveOutputValuesValidation,
      sufficientFundsValidation,
      attestationValidation,
      dataLengthValidation,
      assetEqualFundsValidation,
      assetNoRepeatedUtxosValidation,
      mintingValidation
    )

  /**
   * Verify that this transaction contains at least one input
   */
  private def nonEmptyInputsValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(transaction.inputs.nonEmpty, (), TransactionSyntaxError.EmptyInputs)

  /**
   * Verify that this transaction does not spend the same box more than once
   */
  private def distinctInputsValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    NonEmptyChain
      .fromSeq(
        transaction.inputs
          .groupBy(_.address)
          .collect {
            case (knownIdentifier, inputs) if inputs.size > 1 =>
              TransactionSyntaxError.DuplicateInput(knownIdentifier)
          }
          .toSeq
      )
      .fold(().validNec[TransactionSyntaxError])(_.invalid[Unit])

  /**
   * Verify that this transaction does not contain too many outputs.  A transaction's outputs are referenced by index,
   * but that index must be a Short value.
   */
  private def maximumOutputsCountValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(transaction.outputs.size < Short.MaxValue, (), TransactionSyntaxError.ExcessiveOutputsCount)

  /**
   * Verify that the timestamp of the transaction is positive (greater than or equal to 0).  Transactions _can_ be created
   * in the past.
   */
  private def nonNegativeTimestampValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(
      transaction.datum.event.schedule.timestamp >= 0,
      (),
      TransactionSyntaxError.InvalidTimestamp(transaction.datum.event.schedule.timestamp)
    )

  /**
   * Verify that the schedule of the timestamp contains valid minimum and maximum slot values
   */
  private def scheduleValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(
      transaction.datum.event.schedule.max >= transaction.datum.event.schedule.min &&
      transaction.datum.event.schedule.min >= 0,
      (),
      TransactionSyntaxError.InvalidSchedule(transaction.datum.event.schedule)
    )

  /**
   * Verify that each transaction output contains a positive quantity (where applicable)
   */
  private def positiveOutputValuesValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    transaction.outputs
      .foldMap[ValidatedNec[TransactionSyntaxError, Unit]](output =>
        (output.value.value match {
          case Value.Value.Lvl(v)  => BigInt(v.quantity.value.toByteArray).some
          case Value.Value.Topl(v) => BigInt(v.quantity.value.toByteArray).some
          case Value.Value.Asset(Value.Asset(_, _, Int128(q, _), _, _, _, _, _, _, _)) => BigInt(q.toByteArray).some
          case _                                                                       => none
        }).foldMap((quantity: BigInt) =>
          Validated
            .condNec(
              quantity > BigInt(0),
              (),
              TransactionSyntaxError.NonPositiveOutputValue(output.value): TransactionSyntaxError
            )
        )
      )

  /**
   * Ensure the input value quantities exceed or equal the (non-minting) output value quantities
   */
  private def sufficientFundsValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    quantityBasedValidation(transaction) { f =>
      val filteredInputs = transaction.inputs.map(_.value.value).filter(f.isDefinedAt)
      val filteredOutputs = transaction.outputs.map(_.value.value).filter(f.isDefinedAt)
      val inputsSum = filteredInputs.map(f).sumAll
      val outputsSum = filteredOutputs.map(f).sumAll
      Validated.condNec(
        inputsSum >= outputsSum,
        (),
        TransactionSyntaxError.InsufficientInputFunds(
          filteredInputs.toList,
          filteredOutputs.toList
        ): TransactionSyntaxError
      )
    }

  /**
   * Perform validation based on the quantities of boxes grouped by type
   *
   * @param f an extractor function which retrieves a BigInt from a Box.Value
   */
  private def quantityBasedValidation(transaction: IoTransaction)(
    f: PartialFunction[Value.Value, BigInt] => ValidatedNec[TransactionSyntaxError, Unit]
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    NonEmptyChain(
      // Extract all Token values and their quantities
      f { case Value.Value.Lvl(v) => BigInt(v.quantity.value.toByteArray) },
      f { case Value.Value.Topl(v) => BigInt(v.quantity.value.toByteArray) }
      // Extract all Token Asset values and their quantities. TODO
    ).combineAll

  /**
   * Validates that the attestations for each of the transaction's inputs are valid
   */
  private def attestationValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    transaction.inputs
      .map(input =>
        input.attestation.value match {
          case Attestation.Value.Predicate(Attestation.Predicate(lock, responses, _)) =>
            predicateLockProofTypeValidation(lock, responses)
          // TODO: There is no validation for Attestation types other than Predicate for now
          case _ => ().validNec[TransactionSyntaxError]
        }
      )
      .combineAll

  /**
   * Validates that the proofs associated with each proposition matches the expected _type_ for a Predicate Attestation
   *
   * (i.e. a DigitalSignature Proof that is associated with a HeightRange Proposition, this validation will fail)
   *
   * Preconditions: lock.challenges.length <= responses.length
   */
  private def predicateLockProofTypeValidation(
    lock:      Lock.Predicate,
    responses: Seq[Proof]
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    (lock.challenges zip responses)
      // TODO: Fix `.getRevealed`
      .map(challenge => proofTypeMatch(challenge._1.getRevealed, challenge._2))
      .combineAll

  /**
   * Validate that the type of Proof matches the type of the given Proposition
   * A Proof.Value.Empty type is considered valid for all Proposition types
   */
  private def proofTypeMatch(proposition: Proposition, proof: Proof): ValidatedNec[TransactionSyntaxError, Unit] =
    (proposition.value, proof.value) match {
      case (_, Proof.Value.Empty) =>
        ().validNec[TransactionSyntaxError] // Empty proofs are valid for all Proposition types
      case (Proposition.Value.Locked(_), Proof.Value.Locked(_)) => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.Digest(_), Proof.Value.Digest(_)) => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.DigitalSignature(_), Proof.Value.DigitalSignature(_)) =>
        ().validNec[TransactionSyntaxError]
      case (Proposition.Value.HeightRange(_), Proof.Value.HeightRange(_)) => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.TickRange(_), Proof.Value.TickRange(_))     => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.ExactMatch(_), Proof.Value.ExactMatch(_))   => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.LessThan(_), Proof.Value.LessThan(_))       => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.GreaterThan(_), Proof.Value.GreaterThan(_)) => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.EqualTo(_), Proof.Value.EqualTo(_))         => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.Threshold(_), Proof.Value.Threshold(_))     => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.Not(_), Proof.Value.Not(_))                 => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.And(_), Proof.Value.And(_))                 => ().validNec[TransactionSyntaxError]
      case (Proposition.Value.Or(_), Proof.Value.Or(_))                   => ().validNec[TransactionSyntaxError]
      case _ => TransactionSyntaxError.InvalidProofType(proposition, proof).invalidNec[Unit]
    }

  /**
   * DataLengthValidation validates approved transaction data length, includes proofs
   * @see [[https://topl.atlassian.net/browse/BN-708]]
   * @param transaction transaction
   * @return
   */
  private def dataLengthValidation(
    transaction: IoTransaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(
      transaction.immutable.value.size <= MaxDataLength,
      (),
      TransactionSyntaxError.InvalidDataLength
    )

  /**
   * AssetEqualFundsValidation For each asset: input assets + minted assets == total asset output
   * @param transaction - transaction
   * @return
   */
  private def assetEqualFundsValidation(transaction: IoTransaction): ValidatedNec[TransactionSyntaxError, Unit] = {
    val inputAssets = transaction.inputs.filter(_.value.value.isAsset).map(_.value.value)
    val outputAssets = transaction.outputs.filter(_.value.value.isAsset).map(_.value.value)

    def groupGivenMintedStatements(stm: AssetMintingStatement) =
      transaction.inputs
        .filter(_.address == stm.groupTokenUtxo)
        .filter(_.value.value.isGroup)
        .map(_.value.getGroup)
        .headOption

    def seriesGivenMintedStatements(mintedAsset: AssetMintingStatement) =
      transaction.inputs
        .filter(_.address == mintedAsset.seriesTokenUtxo)
        .filter(_.value.value.isSeries)
        .map(_.value.getSeries)
        .headOption

    val mintedAsset = transaction.mintingStatements.map { stm =>
      val series = seriesGivenMintedStatements(stm)
      Value.defaultInstance
        .withAsset(
          Value.Asset(
            groupId = groupGivenMintedStatements(stm).map(_.groupId),
            seriesId = series.map(_.seriesId),
            quantity = stm.quantity,
            fungibility = series.map(_.fungibility).getOrElse(FungibilityType.GROUP_AND_SERIES)
          )
        )
        .value
    }

    def tupleAndGroup(s: Seq[Value.Value]): Try[Map[ValueTypeIdentifier, BigInt]] =
      Try {
        s.map(value => (value.typeIdentifier, value.quantity: BigInt))
          .groupBy(_._1)
          .view
          .mapValues(_.map(_._2).sum)
          .toMap
      }

    val res = for {
      input  <- tupleAndGroup(inputAssets).toEither
      minted <- tupleAndGroup(mintedAsset).toEither
      output <- tupleAndGroup(outputAssets).toEither
      keySetResult = input.keySet ++ minted.keySet == output.keySet
      compareResult = output.keySet.forall(k =>
        input.getOrElse(k, 0: BigInt) + minted.getOrElse(k, 0) == output.getOrElse(k, 0)
      )
    } yield (keySetResult && compareResult)

    Validated.condNec(
      res.map(_ == true).getOrElse(false),
      (),
      TransactionSyntaxError.InsufficientInputFunds(
        transaction.inputs.map(_.value.value).toList,
        transaction.outputs.map(_.value.value).toList
      )
    )

  }

  /**
   * Asset No Repeated Utxos Validation
   * - For all assets minting statement ams1, ams2, ...,  Should not contain repeated UTXOs
   *
   * @param transaction - transaction
   * @return
   */
  private def assetNoRepeatedUtxosValidation(transaction: IoTransaction): ValidatedNec[TransactionSyntaxError, Unit] =
    NonEmptyChain
      .fromSeq(
        transaction.mintingStatements
          .flatMap(stm => Seq(stm.groupTokenUtxo, stm.seriesTokenUtxo))
          .groupBy(identity)
          .collect {
            case (address, seqAddresses) if seqAddresses.size > 1 =>
              TransactionSyntaxError.DuplicateInput(address)
          }
          .toSeq
      )
      .fold(().validNec[TransactionSyntaxError])(_.invalid[Unit])

  private def mintingInputsProjection(transaction: IoTransaction): Seq[SpentTransactionOutput] =
    transaction.inputs.filter { stxo =>
      !stxo.value.value.isTopl &&
      !stxo.value.value.isAsset &&
      (!stxo.value.value.isLvl || (transaction.groupPolicies.exists(
        _.event.registrationUtxo == stxo.address
      ) || transaction.seriesPolicies.exists(_.event.registrationUtxo == stxo.address)))
    }

  private def mintingOutputsProjection(transaction: IoTransaction): Seq[UnspentTransactionOutput] = {
    val groupIdsOnMintedStatements =
      transaction.inputs
        .filter(_.value.value.isGroup)
        .filter(sto => transaction.mintingStatements.map(_.groupTokenUtxo).contains(sto.address))
        .map(_.value.getGroup.groupId)

    val seriesIdsOnMintedStatements =
      transaction.inputs
        .filter(_.value.value.isSeries)
        .filter(sto => transaction.mintingStatements.map(_.seriesTokenUtxo).contains(sto.address))
        .map(_.value.getSeries.seriesId)

    transaction.outputs.filter { utxo =>
      !utxo.value.value.isLvl &&
      !utxo.value.value.isTopl &&
      (!utxo.value.value.isGroup || transaction.groupPolicies
        .map(_.event.computeId)
        .contains(utxo.value.getGroup.groupId)) &&
      (!utxo.value.value.isSeries || transaction.seriesPolicies
        .map(_.event.computeId)
        .contains(utxo.value.getSeries.seriesId)) &&
      (!utxo.value.value.isAsset || (utxo.value.getAsset.groupId.exists(
        groupIdsOnMintedStatements.contains
      ) && utxo.value.getAsset.seriesId.exists(seriesIdsOnMintedStatements.contains)))
    }
  }

  private def mintingValidation(transaction: IoTransaction) = {
    val projectedTransaction = transaction
      .withInputs(mintingInputsProjection(transaction))
      .withOutputs(mintingOutputsProjection(transaction))

    val groups = projectedTransaction.outputs.filter(_.value.value.isGroup).map(_.value.getGroup)
    val series = projectedTransaction.outputs.filter(_.value.value.isSeries).map(_.value.getSeries)
    val assets = projectedTransaction.outputs.filter(_.value.value.isAsset).map(_.value.getAsset)

    def registrationInPolicyContainsLvls(registrationUtxo: TransactionOutputAddress): Boolean =
      projectedTransaction.inputs.exists { stxo =>
        stxo.value.value.isLvl &&
        stxo.value.getLvl.quantity > 0 &&
        stxo.address == registrationUtxo
      }

    val validGroups = groups.forall { group =>
      transaction.groupPolicies.exists { policy =>
        policy.event.computeId == group.groupId &&
        registrationInPolicyContainsLvls(policy.event.registrationUtxo)
      } &&
      group.quantity > 0
    }

    val validSeries = series.forall { series =>
      transaction.seriesPolicies.exists { policy =>
        policy.event.computeId == series.seriesId &&
        registrationInPolicyContainsLvls(policy.event.registrationUtxo) &&
        series.quantity > 0
      }
    }

    /**
     * Let sIn be the total number of series constructor tokens with identifier in the input,
     * sOut the total number of series constructor tokens with identifier in the output,
     * and burned the number of where the referenced series specifies a token supply, then we have:
     * sIn - burned = sOut
     */
    val validAssets = transaction.mintingStatements.forall { ams =>
      val maybeSeries: Option[Value.Series] =
        transaction.inputs
          .filter(_.value.value.isSeries)
          .filter(_.address == ams.seriesTokenUtxo)
          .map(_.value.getSeries)
          .headOption

      maybeSeries match {
        case Some(s) =>
          s.tokenSupply match {
            case Some(tokenSupplied) =>
              val sIn = transaction.inputs
                .filter(_.value.value.isSeries)
                .filter(_.value.getSeries.seriesId == s.seriesId)
                .map(_.value.getSeries.quantity: BigInt)
                .sum

              val sOut =
                transaction.outputs
                  .filter(_.value.value.isSeries)
                  .filter(_.value.getSeries.seriesId == s.seriesId)
                  .map(_.value.getSeries.quantity: BigInt)
                  .sum

              val burned = sIn - sOut

              // all asset minting statements quantity with the same series id
              def quantity(s: Value.Series) = transaction.mintingStatements.map { ams =>
                val filterSeries = transaction.inputs
                  .filter(_.address == ams.seriesTokenUtxo)
                  .filter(_.value.value.isSeries)
                  .map(_.value.getSeries)
                  .filter(_.seriesId == s.seriesId)
                if (filterSeries.isEmpty) BigInt(0) else ams.quantity: BigInt
              }

              burned * tokenSupplied == quantity(s).sum

            case None => true
          }
        case None => false
      }
    }

    Validated.condNec(
      validGroups && validSeries && validAssets,
      (),
      TransactionSyntaxError.InsufficientInputFunds(
        transaction.inputs.map(_.value.value).toList,
        transaction.outputs.map(_.value.value).toList
      )
    )

  }
}
