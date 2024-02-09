package co.topl.brambl.playground

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import cats.syntax.parallel._
import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.brambl.syntax.{AssetType, valueToTypeIdentifierSyntaxOps}
import co.topl.genus.services.TxoState
import org.bitcoins.commons.jsonmodels.bitcoind.UnspentOutput

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

// TODO: As a second pass, consider garbage collection on the monitoring data (sad paths) (state=spent?)
// TODO: As a third pass, consider threadsafety (removal and addition of addresses while monitoring) (after discussion with edmundo)

case class MonitoringService() {
  var monitoredAddresses: ArrayBuffer[LockAddress] = ArrayBuffer.empty

  def monitorToplAddress(addr: LockAddress): Unit = monitoredAddresses += addr

  def start(assetIdentifier: AssetType, pegInClaimBtc: (TransactionId, LockAddress) => Unit): Unit = {
    while(monitoredAddresses.nonEmpty){
      monitorTbtcClaimed(assetIdentifier, pegInClaimBtc)
        .parSequence
        .unsafeRunTimed(5.seconds)
      Thread.sleep(1000)
    }
  }

  def stop(): Unit = monitoredAddresses.clear()


  // Monitor that the addresses have been spent FROM
  def monitorTbtcClaimed(assetIdentifier: AssetType, pegInClaimBtc: (TransactionId, LockAddress) => Unit): Seq[IO[Unit]] = {

    val queries: ArrayBuffer[IO[Unit]] = monitoredAddresses.zipWithIndex.map({
      // Only querying for spent utxos (indicates the user has claimed the tBTC)
      case (addr, idx) => genusQueryApi.queryUtxo(addr, txoState = TxoState.SPENT)
        // We expect there to be only one tbtc asset at this LockAddress so we can find it/return it as an Option
        .map(txos => (txos.find(_.transactionOutput.value.value.typeIdentifier == assetIdentifier), idx))
        .map({
            case (Some(txo), idx) => {
              pegInClaimBtc(???, addr) // oh shit how to get the TxId
              monitoredAddresses.remove(idx) // No need to keep monitoring
            }
            case (None, _) => ()
          })
        })

      queries.toSeq
    }

  def doMonitorDescriptors(): Unit = {
//    monitoredAddresses.get(addr).foreach(_(addr))
  }



  def handleDescriptorChange(txos: Vector[UnspentOutput]): Unit = ???

  var monitoredDescriptors: ArrayBuffer[UnspentOutput] = ArrayBuffer.empty

  def monitorDescriptor(descriptor: String): Unit = ???
}
