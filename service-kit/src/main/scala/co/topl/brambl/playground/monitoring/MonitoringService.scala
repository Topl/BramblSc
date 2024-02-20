package co.topl.brambl.playground.monitoring

import cats.effect._
import cats.effect.implicits._
import cats.effect.instances.all._
import cats.effect.kernel.Ref
import cats.implicits._
import cats.instances.list._
import co.topl.brambl.models.LockAddress
import co.topl.brambl.playground.ScriptBuilder.PegOut
import co.topl.brambl.playground.monitoring.MonitoringService.ToMonitor
import co.topl.brambl.playground.{Bridge, genusQueryApi, handleCall, rpcCli}
import co.topl.genus.services.TxoState
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{TransactionOutPoint, WitnessTransaction}

import scala.collection.immutable.Queue
import scala.language.implicitConversions

case class MonitoringService(
  bridge:      Bridge,
  pegInDescsTransfer:     ToMonitor[IO, (String, LockAddress)],
  pegInLockAddrsClaim: ToMonitor[IO, LockAddress],
  pegInDescsReclaim:     ToMonitor[IO, String],
  pegOutLockAddrsTransfer:     ToMonitor[IO, (String, LockAddress)],
  pegOutDescsClaim:     ToMonitor[IO, String],
  pegOutLockAddrsReclaim:     ToMonitor[IO, LockAddress],
) {
  def p(msg: String): IO[Unit] = IO.println(s"Monitoring Service: $msg")

  def run(): IO[Nothing] = p("starting up...") *> process()

  def process(): IO[Nothing] = {
    val monitorPegInTransfer = pegInDescsTransfer.take() flatMap {
      case Some((desc, addr)) => checkPegInTransfer(desc, addr)
      case None => IO.unit
    }
    val monitorPegInClaim = pegInLockAddrsClaim.take() flatMap {
      case Some(addr) => checkPegInClaimed(addr)
      case None    => IO.unit
    }
    val monitorPegInReClaim = pegInDescsReclaim.take() flatMap {
      case Some(desc) => checkPegInReclaim(desc)
      case None    => IO.unit
    }
    val monitorPegOutTransfer = pegOutLockAddrsTransfer.take() flatMap {
      case Some((desc, addr)) => checkPegOutTransfer(desc, addr)
      case None    => IO.unit
    }
    Seq(
      monitorPegInTransfer,
      monitorPegInClaim,
      monitorPegInReClaim,
      monitorPegOutTransfer
    ).parSequence.foreverM
  }

  // Monitor that a Bitcoin Descriptor has been funded (transfer BTC)
  def checkPegInTransfer(desc: String, addr: LockAddress): IO[Unit] = {
    val expectedAddr = BitcoinAddress(handleCall(rpcCli.deriveOneAddress(walletName = "bridge-watcher", desc)).get)
    val allUtxos = handleCall(rpcCli.listUnspent(walletName = "bridge-watcher")).get // can add label since only 10 records will return
    val output = allUtxos.find(_.address.get == expectedAddr)
    output match {
      case Some(utxo) =>
        p(s"Descriptor is funded. Starting minting. $desc") *>
        pegInDescsReclaim.add(desc) *> // Now that the descriptor is funded, we should monitor if the funds get reclaimed
        IO(
          bridge.triggerMinting(TransactionOutPoint(utxo.txid, UInt32(utxo.vout)).toHumanReadableString, desc)
        ) *>
        pegInLockAddrsClaim.add(addr).start.void // Now that the Topl address has TBTC, we should monitor if they get claimed
      case None =>
          pegInDescsTransfer.add((desc, addr)).start.void
    }
  }


  // Monitor that a Topl address have been spent FROM (claimed TBTC)
  def checkPegInClaimed(addr: LockAddress): IO[Unit] =
    for {
      // Only querying for spent utxos (indicates the user has claimed the tBTC)
      spent <- genusQueryApi.queryUtxo(addr, txoState = TxoState.SPENT)
      // We expect there to be only one tbtc asset at this LockAddress so we can find it/return it as an Option
      // In production, we would need to check the type of the asset to ensure it is tBTC
      res <- spent.find(_.transactionOutput.value.value.isAsset) match {
        case Some(txo) => // TODO: Handle the case where the bridge reclaimed the TBTC.. if that's the case, then the BTC will not be available to spend anymore
          // TODO: in the future, we will be able to access the STXO of the TXO
          //          IO.pure(bridge.notifyOfTbtcClaim(???, addr))
          p(s"placeholder for claiming BTC $addr").start.void
        case None =>
          pegInLockAddrsClaim.add(addr).start.void // Re-add the address to the monitoring list if it hasn't been claimed
      }
    } yield res
  // Monitor that a Bitcoin Descriptor has been spent from (user reclaims BTC)
  // Preconditions: Descriptor is already funded (has been returned by listUnspent)
  def checkPegInReclaim(desc: String): IO[Unit] = {
    // If we ever get here, then we know the descriptor has been funded
    val expectedAddr = BitcoinAddress(handleCall(rpcCli.deriveOneAddress(walletName = "bridge-watcher", desc)).get)
    val allUtxos = handleCall(rpcCli.listUnspent(walletName = "bridge-watcher")).get
    val output = allUtxos.find(_.address.get == expectedAddr)
    output match {
      case Some(_) =>
          pegInDescsReclaim.add(desc).start.void
      case None =>
        // The descriptor could have either been spent from the user or the bridge
        // The called function will handle the cases
        p(s"Descriptor has been spent from. attempting to reclaim TBTC $desc") *>
          IO(bridge.reclaimTbtc(desc)).start.void
    }
  }

  // Monitor that a Topl Address has been funded (transfer TBTC)
  def checkPegOutTransfer(desc: String, addr: LockAddress): IO[Unit] = for {
    spent <- genusQueryApi.queryUtxo(addr) // Default is unspent
    // In production, we would need to check the type of the asset to ensure it is tBTC
    res <- spent.find(_.transactionOutput.value.value.isAsset) match {
      case Some(txo) =>
        p(s"Topl Address is funded. Starting BTC transafer. $addr") *>
          pegOutLockAddrsReclaim.add(addr) *> // Now that the address is funded, we should monitor if the funds get reclaimed
          IO(bridge.triggerBtcTransfer(txo.outputAddress)) *>
          pegOutDescsClaim.add(desc).start.void // Now that the descriptor has BTC, we should monitor if they get claimed
      case None =>
        pegOutLockAddrsTransfer.add((desc, addr)).start.void // Re-add the address to the monitoring list if it hasn't been funded
    }
  } yield res

  // Monitor that a Bitcoin descriptor have been spent FROM (claimed BTC)
  def checkPegOutClaimed(desc: String): IO[Unit] = {
    // If we ever get here, then we know the descriptor has been funded
    val expectedAddr = BitcoinAddress(handleCall(rpcCli.deriveOneAddress(walletName = "bridge-watcher", desc)).get)
    val allUtxos = handleCall(rpcCli.listUnspent(walletName = "bridge-watcher")).get
    val output = allUtxos.find(_.address.get == expectedAddr)
    output match {
      case Some(_) =>
        pegOutDescsClaim.add(desc).start.void // Re-add the descriptor to the monitoring list if it hasn't been claimed
      case None => //  Has been spent
        {
          val sentProof = handleCall(rpcCli.listSinceBlockWallet("bridge-watcher"))
            .get.transactions.filter(_.category == "send") // All "sent" transactions
            .map(_.txid).toSet
            .map(txId => handleCall(rpcCli.getTransaction(txId, walletNameOpt = Some("bridge-watcher"))).get.hex.asInstanceOf[WitnessTransaction])
            // Find the transaction that spent the descriptor (compare to the witness)
            .map(_.witness.witnesses.find(wit => wit.stack.head == PegOut.descToScriptPubKey(desc).asmBytes))
            .find(_.isDefined).flatten
          sentProof match {
            case Some(proof) =>
              IO.println(s"Found the transaction spending BTC. Claiming TBTC $desc") *>
              IO(bridge.claimTbtc(proof, desc)).start.void
            case None => // We did not find the spending transaction
              IO.println(s"!!!!!!!!! ERROR Could not find the spending transaction for $desc !!!!!!!!!!!") *> // Should never come here
              pegOutDescsClaim.add(desc).start.void
          }
        }
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
