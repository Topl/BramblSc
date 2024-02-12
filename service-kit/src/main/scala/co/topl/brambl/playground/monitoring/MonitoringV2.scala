package co.topl.brambl.playground.monitoring

import cats.effect._
import cats.effect.implicits._
import cats.effect.instances.all._
import cats.effect.std.Console
import cats.instances.list._

import scala.collection.immutable.Queue
import scala.concurrent.duration.DurationInt

object MonitoringV2 extends IOApp {

  object ToMonitor {
    def empty[F[_]: Async, A]: IO[ToMonitor[IO, A]] = Ref.of[IO, Queue[A]](Queue.empty).map(ToMonitor(_))
  }

  case class ToMonitor[F[_]: Async, A](stateR: Ref[IO, Queue[A]]) {
    def add(addr: A): IO[Unit] = Deferred[IO, Unit].flatMap[Unit] { _ =>
      Async[IO].uncancelable { _ =>
        stateR.modify(_.enqueue(addr) -> Async[IO].unit).flatten
      }
    }

    def take(): IO[Option[A]] = Deferred[IO, Int].flatMap { _ =>
      Async[IO].uncancelable { _ =>
        stateR.modify { curState =>
          curState.dequeueOption
            .map(d => d._2 -> Async[IO].pure(Option(d._1)))
            .getOrElse(curState -> Async[IO].pure(Option.empty[A]))
        }.flatten
      }
    }
  }

  case class Service(addrs: ToMonitor[IO, String]) {
    def p(msg: String): IO[Unit] = IO.println(s"Service: $msg")
    def run: IO[FiberIO[Nothing]] = {
      p("starting up").andWait(2.seconds) *>
      p("running infinite loop") *>
        process()
    }
    def process(): IO[FiberIO[Nothing]] = {
      val x = for {
        addr <- addrs.take()
        res <- {
          addr match {
              case Some(a) => p(s"processing $a").andWait(2.seconds)
              case None => p("no address to process").andWait(2.seconds)
          }
        }
      } yield res
      x.foreverM.start
    }
  }

  case class Bridge(addrs: ToMonitor[IO, String]) {
    def p(msg: String): IO[Unit] = IO.println(s"Bridge: $msg")
    def run: IO[Unit] = {
      (for {
        service <- Service(addrs).run
        bridge <- bridgeActivities().start
        res <- bridge.join *> service.cancel.start
      } yield res.joinWithUnit).flatten
    }
    def bridgeActivities(): IO[Unit] = {
      p("starting up") *>
        p("adding TEST") *>
        addrs.add("TEST").andWait(1.seconds) *>
        p("adding SECRET") *>
        addrs.add("SECRET").andWait(1.seconds) *>
        p("adding RANDOM") *>
        addrs.add("RANDOM").andWait(1.seconds) *>
        p("stopping in 3 seconds").andWait(3.seconds)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      addrs <- ToMonitor.empty[IO, String] // Shared state
      bridge = Bridge(addrs)
      res
        <- bridge.run.as(ExitCode.Success) // Run bridge and monitoring service in parallel until done (likely by user cancelling with CTRL-C)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res

  }
}