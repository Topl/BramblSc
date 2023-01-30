package co.topl.brambl.validation

import cats.Monad
import cats.implicits._
import cats.data.{Chain, ValidatedNec}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier

object TransactionSyntaxInterpreter {
  def make[F[_]: Monad](): TransactionSyntaxVerifier[F] = new TransactionSyntaxVerifier[F] {
    override def validate(t: IoTransaction): F[Either[TransactionSyntaxError, IoTransaction]] =
      validators.foldMap(_ apply t)
        .toEither
        // TODO: The following line is a temporary measure until we decide on a return type
        .leftMap[TransactionSyntaxError](_ => TransactionSyntaxError.SyntaxFailed)
        .as(t)
        .pure[F]
  }

  private val validators: Chain[IoTransaction => ValidatedNec[TransactionSyntaxError, Unit]] =
    Chain(
      ???
    )
}
