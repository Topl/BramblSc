package co.topl.brambl.playground.monitoring

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.models.LockAddress
import co.topl.brambl.playground.monitoring.MonitoringService.ToMonitor
import co.topl.brambl.playground.{genusQueryApi, Alice, Bridge, BridgeQuery}

object Example extends IOApp {

  case class Example(pegInLockAddrs: ToMonitor[IO, LockAddress], pegInDescs: ToMonitor[IO, String], pegInDescsReclaim: ToMonitor[IO, String]) {

    def run: IO[Unit] = {
      val bridge = Bridge()
      val bridgeRpc = BridgeQuery(bridge, pegInLockAddrs, pegInDescs)
      val alice = Alice(bridgeRpc)
      (for {
        service <- MonitoringService(bridgeRpc, pegInLockAddrs, pegInDescs, pegInDescsReclaim).run().start
        demoRes <- demo(bridgeRpc, alice).start
        res     <- demoRes.join *> service.cancel.start
      } yield res.joinWithUnit).flatten
    }

    def demo(bridgeRpc: BridgeQuery, alice: Alice): IO[Unit] = {
      // Try splitting it up, not in an IO Pure, but chaning the IOs (multiple IO pures)
      val transferTo = for {
        resp <- {
          println("> Alice initiating peg-in...")
          IO.pure(alice.initiatePegIn())
        }
        desc = resp.desc
        sendRes <- {
          println(s"> Alice sending BTC to $desc...")
          IO.pure(alice.btcWallet.sendBtcToDesc(desc)) *> IO.pure(resp.toplAddress)
        }
      } yield sendRes
      val x = for {
        lockAddr <- transferTo
        txId <- (IO.println(s"> Alice waiting for tBTC at ${lockAddr.toBase58()} to be funded...") *>
        genusQueryApi.queryUtxo(lockAddr).iterateWhile(_.isEmpty) *> IO.println("tBTC funded!") *>
        IO.println(s"> Alice claiming tBTC at ${lockAddr.toBase58()}...") *>
        IO(alice.claimTBtc(lockAddr)))
      } yield (txId, lockAddr)
      x.map { res =>
        println("> notifying bridge of tBTC claim...")
        bridgeRpc.notifyOfTbtcClaim(res._1, res._2)
      }.void
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      pegInLockAddrs <- ToMonitor.empty[IO, LockAddress] // Shared state
      pegInDescs     <- ToMonitor.empty[IO, String] // Shared state
      pegInDescsReclaim     <- ToMonitor.empty[IO, String] // Shared state
      bridge = Example(pegInLockAddrs, pegInDescs, pegInDescsReclaim) // Create bridge
      res <- bridge.run
        .as(
          ExitCode.Success
        ) // Run bridge and monitoring service in parallel until done (likely by user cancelling with CTRL-C)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res

}
