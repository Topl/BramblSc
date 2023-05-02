package co.topl.brambl.builders

/**
 * A generic error type that is returned by the builders when
 * a build is unsuccessful.
 *
 * @param message The error message
 */
abstract class BuilderError(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

object BuilderError {

  /**
   * A Builder error indicating that an IoTransaction's input
   * ([[co.topl.brambl.models.transaction.SpentTransactionOutput SpentTransactionOutput]])
   * was not successfully built.
   *
   * @param message The error message indicating why the build is unsuccessful
   */
  case class InputBuilderError(message: String, cause: Throwable = null) extends BuilderError(message, cause)

  /**
   * A Builder error indicating that a IoTransaction's output
   * ([[co.topl.brambl.models.transaction.UnspentTransactionOutput UnspentTransactionOutput]])
   * was not successfully built.
   *
   * @param message The error message indicating why the build is unsuccessful
   */
  case class OutputBuilderError(message: String, cause: Throwable = null) extends BuilderError(message, cause)
}
