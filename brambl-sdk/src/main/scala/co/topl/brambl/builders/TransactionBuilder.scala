package co.topl.brambl.builders

import co.topl.brambl.models.KnownIdentifier.{TransactionOutput32, TransactionOutput64}
import co.topl.brambl.models.builders.{InputBuildRequest, OutputBuildRequest}
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import com.google.protobuf.ByteString
import quivr.models.SmallData

/**
 * Defines a builder for [[IoTransaction]]s
 */
trait TransactionBuilder[F[_]] {

  /**
   * Construct an unproven IoTransaction input ([[SpentTransactionOutput]]).
   * A SpentTransactionOutput spends an existing [[UnspentTransactionOutput]].
   *
   * @param data The data required to build a SpentTransactionOutput
   *             The data is an object with the following fields:
   *             id: KnownIdentifier - Identifies an existing IoTransaction output for which the built input is spending.
   *             metadata: Option[SmallData] - Optional metadata to include in the built SpentTransactionOutput
   *             If not provided, the built SpentTransactionOutput's metadata will be empty data
   * @return Either a InputBuilderError or the built SpentTransactionOutput
   */
  def constructUnprovenInput(data: InputBuildRequest): F[Either[BuilderError.InputBuilderError, SpentTransactionOutput]]

  /**
   * Construct a IoTransaction output ([[UnspentTransactionOutput]]).
   *
   * @param data The data required to build an UnspentTransactionOutput
   *             The data is an object with the following fields:
   *             lock: Lock - The lock for the built UnspentTransactionOutput. It will be encoded in the address
   *             value: Value - The value for the built UnspentTransactionOutput
   *             metadata: Option[SmallData] - Optional metadata to include in the built UnspentTransactionOutput
   *             If not provided, the built UnspentTransactionOutput's metadata will be empty data
   * @return Either a OutputBuilderError or the built UnspentTransactionOutput
   */
  def constructOutput(data: OutputBuildRequest): F[Either[BuilderError.OutputBuilderError, UnspentTransactionOutput]]

  /**
   * Construct an unproven [[IoTransaction]]. The transaction fee is whatever is left over after the sum of the
   * LVL outputs is subtracted from the sum of the LVL inputs. Consequently, any change needs to be explicitly added as an output.
   *
   * A [[SpentTransactionOutput]] spends an existing
   * [[UnspentTransactionOutput]].
   *
   * @param inputRequests A list of data required to build the inputs of this IoTransaction
   *                      Each element represents a single input.
   * @param outputRequests A list of data required to build the outputs of this IoTransaction.
   *                       Each element represents a single output.
   * @param schedule The schedule for this IoTransaction
   *                 If not provided, the built transaction will have a default schedule with min set to 0, max set to
   *                 2147483647 and timestamp set to the current time.
   * TODO: when the slot number conversion is working, default min will be set to the current slot number and max set to the current slot number + 14400 (approximately 4 hours later)
   * @param output32Refs A list of identifiers that refer to existing IoTransactions outputs using 32 byte evidences.
   *                     Defaults to an empty list
   * @param output64Refs A list of identifiers that refer to existing IoTransactions outputs using 64 byte evidences.
   *                     Defaults to an empty list
   * @param metadata Optional metadata to include with the built transaction
   *                 If not provided, the built transaction's metadata will be empty data
   *
   * @return Either a list of BuilderError or the built IoTransaction
   */
  def constructUnprovenTransaction(
    inputRequests:  List[InputBuildRequest],
    outputRequests: List[OutputBuildRequest],
    schedule:       Option[Schedule] = None,
    output32Refs:   List[TransactionOutput32] = List(),
    output64Refs:   List[TransactionOutput64] = List(),
    metadata:       Option[SmallData] = None
  ): F[Either[List[BuilderError], IoTransaction]]
}
