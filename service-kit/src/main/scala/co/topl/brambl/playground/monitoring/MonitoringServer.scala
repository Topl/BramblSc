package co.topl.brambl.playground.monitoring

import cats.effect.{IO, MonadCancel, Spawn}

import scala.concurrent.duration.DurationInt
import cats.effect.syntax.all._
import cats.syntax.all._

object MonitoringServer {
  case class MonitoringConnection() extends Connection[IO] {
    def read: IO[Array[Byte]] = IO.println("reading bytes").andWait(2.seconds).map(_ => "bytes".getBytes)
    def write(bytes: Array[Byte]): IO[Unit] = IO.println("writing bytes").andWait(2.seconds).map(_ => println(new String(bytes)))
    def close: IO[Unit] = IO.println("writing bytes").andWait(2.seconds).map(_ => println("closed"))
  }
  def apply(): Server[IO] = new Server[IO] {
      def accept: IO[MonitoringConnection] = IO.println("accepting connection").andWait(2.seconds).map(_ => MonitoringConnection())
  }

  def endpoint[F[_] : Spawn](
                              server: Server[F])(
                              body: Array[Byte] => F[Array[Byte]])
  : F[Unit] = {

    def handle(conn: Connection[F]): F[Unit] =
      for {
        request <- conn.read
        response <- body(request)
        _ <- conn.write(response)
      } yield ()

    val handler = MonadCancel[F] uncancelable { poll =>
      poll(server.accept) flatMap { conn =>
        handle(conn).guarantee(conn.close).start
      }
    }

    handler.foreverM
  }
}
