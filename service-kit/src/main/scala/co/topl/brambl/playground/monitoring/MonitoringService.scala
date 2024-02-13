package co.topl.brambl.playground.monitoring

import cats.effect._
import cats.effect.implicits._
import cats.effect.instances.all._
import cats.effect.kernel.Ref
import cats.implicits._
import cats.instances.list._
import co.topl.brambl.models.LockAddress
import co.topl.brambl.playground.monitoring.MonitoringService.ToMonitor
import co.topl.brambl.playground.{genusQueryApi, handleCall, rpcCli, BridgeQuery}
import co.topl.genus.services.TxoState
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.TransactionOutPoint

import scala.collection.immutable.Queue
import scala.language.implicitConversions

// TODO: As a second pass, consider garbage collection on the monitoring data (sad paths) (state=spent?)

case class MonitoringService(
  bridgeRpc:      BridgeQuery,
  pegInLockAddrs: ToMonitor[IO, LockAddress],
  pegInDescs:     ToMonitor[IO, String]
) {
  def p(msg: String): IO[Unit] = IO.println(s"Monitoring Service: $msg")

  def run(): IO[Nothing] =
    p("starting up monitoring service...") *>
      p("running infinite loop") *>
      process()

  def process(): IO[Nothing] = {
    val p1 = pegInLockAddrs.take() flatMap {
      case Some(a) => checkPegInClaimed(a)
      case None    => IO.unit
    }
    val p2 = pegInDescs.take() flatMap {
      case Some(a) => checkPegInTransfer(a)
      case None    => IO.unit
    }
    Seq(p1, p2).parSequence.foreverM
  }

  // Monitor that a Topl address have been spent FROM (claimed TBTC)
  def checkPegInClaimed(addr: LockAddress): IO[Unit] =
    for {
      // Only querying for spent utxos (indicates the user has claimed the tBTC)
      spent <- genusQueryApi.queryUtxo(addr, txoState = TxoState.SPENT)
      // We expect there to be only one tbtc asset at this LockAddress so we can find it/return it as an Option
      // In production, we would need to check the type of the asset to ensure it is tBTC
      res <- spent.find(_.transactionOutput.value.value.isAsset) match {
        case Some(txo) =>
          // TODO: in the future, we will be able to access the STXO of the TXO
          //          IO.pure(bridge.notifyOfTbtcClaim(???, addr))
          p(s"placeholder for claiming BTC $addr").start.void
        case None => // p(s"LockAddress is not yet claimed. re-adding to queue $addr") *> // to declutter logs
          pegInLockAddrs.add(addr).start.void // Re-add the address to the monitoring list if it hasn't been claimed
      }
    } yield res

  // Monitor that a Bitcoin Descriptor has been funded (transfer BTC)
  def checkPegInTransfer(desc: String): IO[Unit] = {
    val expectedAddr = BitcoinAddress(handleCall(rpcCli.deriveOneAddress(walletName = "bridge-watcher", desc)).get)
    val allUtxos = handleCall(rpcCli.listUnspent(walletName = "bridge-watcher")).get
    val output = allUtxos.find(_.address.get == expectedAddr)
    output match {
      case Some(utxo) =>
        p(s"Descriptor is funded. Starting minting. $desc") *>
        IO(
          bridgeRpc.notifyOfBtcTransfer(TransactionOutPoint(utxo.txid, UInt32(utxo.vout)).toHumanReadableString, desc)
        ).start.void
      case None =>
        p(s"Descriptor is not yet funded. re-adding to queue $desc") *>
        pegInDescs.add(desc).start.void
    }
  }
}

object MonitoringService {

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
}
