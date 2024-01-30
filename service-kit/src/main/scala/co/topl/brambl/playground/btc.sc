import co.topl.brambl.models.TransactionId
import co.topl.brambl.playground._
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto._

val DoesBridgeClaim: Boolean = true

setUpWallets()

val alice = Alice()
val bridge = Bridge()

val desc = alice.initiateRequest(bridge)
alice.sendBtcToDesc(desc)

mineBlocks(1)

def extractSecret(txId: TransactionId): String = {
  // POC of how to extract secret from a txId goes here
  alice.BtcCtx.secret.getOrElse("fake secret")
}

if(DoesBridgeClaim) {
  val lockAddr = alice.informOfBitcoinTx(bridge) // causes minting of tBtc to lockAddr, until monitoring
  val toplTxId = alice.claimTBtc(lockAddr)
  // The following is manual, but should be automated in the future (via monitoring)
  val secret = extractSecret(toplTxId)
  bridge.claimBtc(secret)
} else {
  println("> Alice waiting 1000 blocks...")  // Alice can only reclaim after 1000 blocks
  mineBlocks(1000)
  alice.reclaimBtc()
}
mineBlocks(1)