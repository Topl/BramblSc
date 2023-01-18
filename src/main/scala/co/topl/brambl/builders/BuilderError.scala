package co.topl.brambl.builders

/**
 * A generic error type that is returned by the builders when
 * a build is unsuccessful.
 *
 * @param message The error message
 */
abstract class BuilderError(val message: String)

object BuilderError {
  /**
   * A Builder error indicating that an IoTransaction's input
   * ([[co.topl.brambl.models.transaction.SpentTransactionOutput SpentTransactionOutput]])
   * was not successfully built.
   *
   * @param message The error message indicating why the build is unsuccessful
   */
  case class InputBuilderError(override val message: String) extends BuilderError(message)

  /**
   * A Builder error indicating that a IoTransaction's output
   * ([[co.topl.brambl.models.transaction.UnspentTransactionOutput UnspentTransactionOutput]])
   * was not successfully built.
   *
   * @param message The error message indicating why the build is unsuccessful
   */
  case class OutputBuilderError(override val message: String) extends BuilderError(message)
}