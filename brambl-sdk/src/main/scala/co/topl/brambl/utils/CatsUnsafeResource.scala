package co.topl.brambl.utils

import cats.data.OptionT
import cats.effect.implicits._
import cats.effect.std.Queue
import cats.effect.{Async, Resource, Sync}
import cats.implicits._

/**
 * Manages thread-unsafe resources in a thread-safe manner by utilizing a cats-effect queue. Inactive resources
 * sit in a queue.  As requests arrive, a resource is dequeued, used, and then requeued.
 */
object CatsUnsafeResource {

  def make[F[_]: Async, T](init: => T, maxParallelism: Int): F[Resource[F, T]] =
    for {
      _     <- Async[F].raiseWhen(maxParallelism < 1)(new IllegalArgumentException("Invalid maxParallelism"))
      queue <- Queue.unbounded[F, Option[T]]
      // Launch with several uninitialized resources
      _ <- queue.offer(None).replicateA(maxParallelism)
      res = Resource.make(
        Async[F].cede *>
        Sync[F].defer(
          OptionT(queue.take)
            // If an uninitialized resource was pulled, initialize it
            .getOrElseF(Async[F].delay(init).guarantee(Async[F].cede))
        )
      )(t => Sync[F].defer(queue.offer(t.some)).guarantee(Async[F].cede))
    } yield res
}
