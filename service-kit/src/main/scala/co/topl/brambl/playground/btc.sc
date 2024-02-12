import co.topl.brambl.playground._
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto._

val PegInHappyPath: Boolean = true

val bridge = Bridge()
val bridgeRpc = BridgeQuery(bridge)
val alice = Alice(bridgeRpc)

def pegIn(): Boolean = {
  println("> Alice initiating peg-in...")
  val resp = alice.initiatePegIn()
  val desc = resp.desc
  println(s"> Alice sending BTC to $desc...")
  val txOut = alice.btcWallet.sendBtcToDesc(desc)
  if(PegInHappyPath) {
    println("> notifying bridge of BTC transfer...")
    val lockAddr = bridgeRpc.notifyOfBtcTransfer(txOut, desc)
    println(s"> Alice claiming tBTC at $lockAddr...")
    val txId = alice.claimTBtc(lockAddr)
    println("> notifying bridge of tBTC claim...")
    bridgeRpc.notifyOfTbtcClaim(txId, lockAddr)
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

if (pegIn()) pegOut()

bridge.monitoringService.stop()