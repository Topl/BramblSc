import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.models.LockAddress
import co.topl.brambl.playground._
import co.topl.brambl.playground.monitoring.MonitoringService.ToMonitor
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto._

val PegInHappyPath: Boolean = true

val pegInLockAddrs = ToMonitor.empty[IO, LockAddress].unsafeRunSync() // trivial for this example
val pegInDescs = ToMonitor.empty[IO, String].unsafeRunSync() // trivial for this example

val bridge = Bridge()
val bridgeRpc = BridgeQuery(bridge, pegInLockAddrs, pegInDescs)
val alice = Alice(bridgeRpc)

def pegIn(): Boolean = {
  println("> Alice initiating peg-in...")
  val resp = alice.initiatePegIn()
  val desc = resp.desc
  println(s"> Alice sending BTC to $desc...")
  val txOut = alice.btcWallet.sendBtcToDesc(desc)
  // bridge monitoring desc
  // user funds desc
  // bridge should still monitor desc in a diff context, if desc has been spent from, then bridge should relcaim the minted tbtc
  if(PegInHappyPath) {
    println("> notifying bridge of BTC transfer...")
    val lockAddr = bridgeRpc.notifyOfBtcTransfer(txOut, desc)
    Thread.sleep(17000)
    println(s"> Alice claiming tBTC at ${resp.toplAddress.toBase58()}...")
    val txId = alice.claimTBtc(resp.toplAddress)
    println("> notifying bridge of tBTC claim...")
    bridgeRpc.notifyOfTbtcClaim(txId, resp.toplAddress)
  } else {
    // Alice opts out (reclaims BTC)
    println("> Alice waiting 1000 blocks...") // Alice can only reclaim after 1000 blocks
    mineBlocks(1000)
    alice.reclaimBtc(desc)
  }
  PegInHappyPath
}

def pegOut(): Unit = {
  println("> Alice initiating peg-out...")
  val resp = alice.initiatePegOut()
  val lock = resp.toplLock
  val desc = resp.desc
  println(s"> Alice sending tBTC to $lock...")
  val utxoId = alice.sendTbtcToAddress(lock)
  println("> notifying bridge of tBTC transfer...")
  bridgeRpc.notifyOfTbtcTransfer(utxoId)
  println(s"> Alice claiming BTC at $desc...")
  val txId = alice.claimBtc(desc)
  println("> notifying bridge of BTC claim...")
  bridgeRpc.notifyOfBtcClaim(txId, desc)
}

println("starting peg-in...")

if (pegIn()) pegOut()
