package co.topl.brambl.playground

import cats.effect.unsafe.implicits.global
import cats.effect.{Fiber, IO}
import cats.implicits._
import cats.syntax.parallel._
import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.brambl.syntax.{AssetType, valueToTypeIdentifierSyntaxOps}
import co.topl.genus.services.TxoState
import org.bitcoins.commons.jsonmodels.bitcoind.UnspentOutput

import java.util.concurrent.Executors
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

// TODO: As a second pass, consider garbage collection on the monitoring data (sad paths) (state=spent?)
// TODO: As a third pass, consider threadsafety (removal and addition of addresses while monitoring) (after discussion with edmundo)

case class MonitoringService() {
  var monitoredAddresses: ArrayBuffer[LockAddress] = ArrayBuffer.empty

  def monitorToplAddress(addr: LockAddress): Unit = monitoredAddresses += addr

  var toplMonitorTask: Option[IO[Fiber[IO, Throwable, Unit]]] = None

  def start(assetIdentifier: AssetType, pegInClaimBtc: (TransactionId, LockAddress) => Unit): Unit = {
    monitoredAddresses
    // We want to run the monitoring in a separate thread
    val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool()) //create other execution context
    toplMonitorTask = IO.pure(
      while (true) { // Non-terminating loop on the monitoring thread
        monitorTbtcClaimed(assetIdentifier, pegInClaimBtc).unsafeRunTimed(5.seconds) // blocking call on the monitoring thread
        Thread.sleep(1000) // sleep for a second on the monitoring thread
      }
    ).startOn(ec).some
  }

  def stop(): Unit = {
    // Cancel the fiber
    toplMonitorTask.get.map(_.cancel)
  }


  // Monitor that the addresses have been spent FROM
  def monitorTbtcClaimed(assetIdentifier: AssetType, pegInClaimBtc: (TransactionId, LockAddress) => Unit): IO[Seq[Unit]] = {

    val queries: ArrayBuffer[IO[Unit]] = monitoredAddresses.zipWithIndex.map({
      // Only querying for spent utxos (indicates the user has claimed the tBTC)
      case (addr, idx) => genusQueryApi.queryUtxo(addr, txoState = TxoState.SPENT)
        // We expect there to be only one tbtc asset at this LockAddress so we can find it/return it as an Option
        .map(txos => (txos.find(_.transactionOutput.value.value.typeIdentifier == assetIdentifier), idx))
        .map({
            case (Some(txo), idx) => {
              pegInClaimBtc(???, addr) // TODO: in the future, we will be able to access the STXO of the TXO
              monitoredAddresses.remove(idx) // No need to keep monitoring
            }
            case (None, _) => ()
          })
        })

//      queries.toSeq.parSequence
    ???
    }

  def doMonitorDescriptors(): Unit = {
//    monitoredAddresses.get(addr).foreach(_(addr))
  }



  def handleDescriptorChange(txos: Vector[UnspentOutput]): Unit = ???

  var monitoredDescriptors: ArrayBuffer[UnspentOutput] = ArrayBuffer.empty

  def monitorDescriptor(descriptor: String): Unit = ???
}
