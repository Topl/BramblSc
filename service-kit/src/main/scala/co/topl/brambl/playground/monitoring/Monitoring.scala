package co.topl.brambl.playground.monitoring

import cats.effect._
import cats.effect.implicits._
import cats.effect.instances.all._
import cats.instances.list._

object Monitoring {
//
//  object ToMonitor {
//    def empty[F[_]: Async, A]: IO[ToMonitor[IO, A]] = Ref.of[IO, Queue[A]](Queue.empty).map(ToMonitor(_))
//  }
//
//  case class ToMonitor[F[_]: Async, A](stateR: Ref[IO, Queue[A]]) {
//    def add(addr: A): IO[Unit] = {
//      Deferred[IO, Unit].flatMap[Unit] { _ =>
//        Async[IO].uncancelable { _ =>
//          stateR.modify(_.enqueue(addr) -> Async[IO].unit).flatten
//        }
//      }
//    }
//
//    def take(): IO[Option[A]] = Deferred[IO, Int].flatMap { _ =>
//      Async[IO].uncancelable { _ =>
//        stateR.modify { curState =>
//          curState.dequeueOption
//            .map(d => d._2 -> Async[IO].pure(Option(d._1)))
//            .getOrElse(curState -> Async[IO].pure(Option.empty[A]))
//        }.flatten
//      }
//    }
//  }
//
//  case class Service(addrs1: ToMonitor[IO, String], addrs2: ToMonitor[IO, String]) {
//    def p(msg: String): IO[Unit] = IO.println(s"Service: $msg")
//    def run: IO[FiberIO[Nothing]] = {
//      p("starting up").andWait(2.seconds) *>
//      p("running infinite loop") *>
//        process()
//    }
//    def process(): IO[FiberIO[Nothing]] = {
//      val p1 = for {
//        addr <- addrs1.take()
//        res <- {
//          addr match {
//              case Some(a) => p(s"1: processing $a").andWait(2.seconds)
//              case None => p("1: no address to process").andWait(2.seconds)
//          }
//        }
//      } yield res
//      val p2 = for {
//        addr <- addrs1.take()
//        res <- {
//          addr match {
//              case Some(a) => p(s"2: processing $a").andWait(2.seconds)
//              case None => p("2: no address to process").andWait(2.seconds)
//          }
//        }
//      } yield res
//      Seq(p1, p2).parSequence.foreverM.start
//    }
//  }
//
//  case class Bridge(addrs1: ToMonitor[IO, String], addrs2: ToMonitor[IO, String]) {
//    def p(msg: String): IO[Unit] = IO.println(s"Bridge: $msg")
//    def run: IO[Unit] = {
//      (for {
//        service <- Service(addrs1, addrs2).run
//        bridge <- bridgeActivities().start
//        res <- bridge.join *> service.cancel.start
//      } yield res.joinWithUnit).flatten
//    }
//    def bridgeActivities(): IO[Unit] = {
//      p("starting up") *>
//        p("adding TEST") *>
//        addrs1.add("TEST").andWait(1.seconds) *>
//        p("adding SECRET") *>
//        addrs2.add("SECRET").andWait(1.seconds) *>
//        p("adding RANDOM") *>
//        addrs1.add("RANDOM").andWait(1.seconds) *>
//        p("stopping in 3 seconds").andWait(3.seconds)
//    }
//  }

//  override def run(args: List[String]): IO[ExitCode] = {
//    for {
//      addrs1 <- ToMonitor.empty[IO, String] // Shared state
//      addrs2 <- ToMonitor.empty[IO, String] // Shared state
//      bridge = Bridge(addrs1, addrs2) // Create bridge
//      res
//        <- bridge.run.as(ExitCode.Success) // Run bridge and monitoring service in parallel until done (likely by user cancelling with CTRL-C)
//        .handleErrorWith { t =>
//          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
//        }
//    } yield res
//
//  }
}
