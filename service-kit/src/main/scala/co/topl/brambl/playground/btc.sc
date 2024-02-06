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
  val desc = alice.initiatePegIn()
  val txOut = alice.sendBtcToDesc(desc)
  if(PegInHappyPath) {
    val lockAddr = bridgeRpc.notifyOfBtcTransfer(txOut)
    val txId = alice.claimTBtc(lockAddr)
    bridgeRpc.notifyOfTbtcClaim(txId, desc)
  } else {
    // Alice opts out (reclaims BTC)
    println("> Alice waiting 1000 blocks...") // Alice can only reclaim after 1000 blocks
    mineBlocks(1000)
    alice.reclaimBtc(desc)
  }
  PegInHappyPath
}

def pegOut(): Unit = {
  val lock = alice.initiatePegOut()
  val utxoId = alice.sendTbtcToAddress(lock)
  val desc = bridgeRpc.notifyOfTbtcTransfer(utxoId)
  val txId = alice.claimBtc(desc)
  bridgeRpc.notifyOfBtcClaim(txId, desc)
}

if (pegIn()) pegOut()