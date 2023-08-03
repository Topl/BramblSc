package co.topl.common

trait ContextlessValidation[F[_], E, T] {

  /**
   * Determines the validity of the given value, scoped without any contextual information
   * (i.e. if T is a Transaction, there is no context about previous transactions or blocks)
   * Usually used for syntactic validation purposes.
   */
  def validate(t: T): F[Either[E, T]]
}
