package co.topl.brambl.builders

/**
 * A generic error type that is returned by the builders when
 * a build is unsuccessful.
 *
 * @param message The error message
 */
abstract class BuilderError(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
