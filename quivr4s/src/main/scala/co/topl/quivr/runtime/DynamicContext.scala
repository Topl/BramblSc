package co.topl.quivr.runtime

import cats.Monad
import cats.data.EitherT
import co.topl.common.ParsableDataInterface
import co.topl.quivr.algebras.{DigestVerifier, SignatureVerifier}
import co.topl.quivr.runtime.QuivrRuntimeErrors.{ContextError, ValidationError}
import quivr.models._

/**
 * The context that will be provided during evaluation of Quivr protocols. This context provides generic interfaces for digest and signature verificiation
 * . Additionally, a data persing interface may do constructed for retrieving and parsing values not currently supported by Quivr
 * @tparam F execution context of the runtime
 * @tparam K the key type that will be used to lookup values in the generic interface maps
 */
trait DynamicContext[F[_], K, Datum] {
  val datums: K => Option[Datum]

  val interfaces: Map[K, ParsableDataInterface]
  val signingRoutines: Map[K, SignatureVerifier[F]]
  val hashingRoutines: Map[K, DigestVerifier[F]]

  def signableBytes: F[SignableBytes]

  def currentTick: F[Long]

  def heightOf(label: K): F[Option[Long]]

  def digestVerify(routine: K)(verification: DigestVerification)(implicit
    monad: Monad[F]
  ): EitherT[F, QuivrRuntimeError, DigestVerification] = for {
    verifier <- EitherT.fromOption[F](hashingRoutines.get(routine), ContextError.FailedToFindDigestVerifier)
    res      <- EitherT(verifier.validate(verification))
  } yield res

  def signatureVerify(routine: K)(verification: SignatureVerification)(implicit
    monad: Monad[F]
  ): EitherT[F, QuivrRuntimeError, SignatureVerification] = for {
    verifier <- EitherT.fromOption[F](signingRoutines.get(routine), ContextError.FailedToFindSignatureVerifier)
    res      <- EitherT(verifier.validate(verification))
  } yield res

  def useInterface[E, T](label: K)(f: Data => Either[QuivrRuntimeError, T])(ff: T => Boolean)(implicit
    monad: Monad[F]
  ): EitherT[F, QuivrRuntimeError, T] =
    EitherT.fromEither[F](
      interfaces.get(label) match {
        case Some(interface) =>
          interface.parse[QuivrRuntimeError, T](f).flatMap { t =>
            Either.cond(ff(t), t, ValidationError.UserProvidedInterfaceFailure)
          }
        case None => Left(ContextError.FailedToFindInterface)
      }
    )

  def exactMatch(label: K, compareTo: Array[Byte])(implicit
    monad: Monad[F]
  ): EitherT[F, QuivrRuntimeError, Array[Byte]] =
    useInterface(label)(d => Right(d.value))(b => b.toByteArray sameElements compareTo)(monad)
      .map(_.toByteArray)

  def lessThan(label: K, compareTo: BigInt)(implicit
    monad: Monad[F]
  ): EitherT[F, QuivrRuntimeError, BigInt] =
    useInterface(label)(d => Right(BigInt(d.value.toByteArray)))(n => n < compareTo)

  def greaterThan(label: K, compareTo: BigInt)(implicit
    monad: Monad[F]
  ): EitherT[F, QuivrRuntimeError, BigInt] =
    useInterface(label)(d => Right(BigInt(d.value.toByteArray)))(n => n > compareTo)

  def equalTo(label: K, compareTo: BigInt)(implicit
    monad: Monad[F]
  ): EitherT[F, QuivrRuntimeError, BigInt] =
    useInterface(label)(d => Right(BigInt(d.value.toByteArray)))(n => n == compareTo)

}
