import cats.effect.unsafe.implicits.global
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.brambl.playground._
import co.topl.brambl.utils.Encoding
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto._

// TODO: topl slot vs bitcoin block; syncing the timing.

val PegInHappyPath: Boolean = true
val PegOutHappyPath: Boolean = true

def extractSecret(txId: TransactionId)(targetLockAddr: LockAddress): String = {
  val tx = bifrostQuery.fetchTransaction(txId).unsafeRunSync().get
  val attestation = tx.inputs.map(_.attestation.getPredicate)
    .find(att => targetLockAddr == txBuilder.lockAddress(Lock().withPredicate(att.lock)).unsafeRunSync())
    .get
  // The following is possible because we know the exact structure of the attestation
  val aliceProof = attestation.responses.head.getAnd
  val preimage = aliceProof.right.getDigest.preimage
  Encoding.encodeToHex(preimage.input.toByteArray ++ preimage.salt.toByteArray)
}

val bridge = Bridge(extractSecret)
val alice = Alice(BridgeQuery(bridge))

def pegIn(): Boolean = {
  val desc = alice.initiatePegIn()
  val txOut = alice.sendBtcToDesc(desc)
  if(PegInHappyPath) {
    val lockAddr = alice.notifyBridgeBtcTransfer(txOut)
    val txId = alice.claimTBtc(lockAddr)
     alice.notifyBridgeTbtcClaim(txId, desc)
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
  val txId = alice.sendTbtcToAddress(lock)
  if (PegOutHappyPath) {
    val desc = alice.notifyBridgeTbtcTransfer()
    val txOut = alice.claimBtc(desc)
    alice.notifyBridgeBtcClaim(txOut)
  } else {
    // Alice opts out (reclaims tBTC)
    // TODO
  }
}

val pegInComplete = pegIn()
if (pegInComplete) {
  pegOut()
}