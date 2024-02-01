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
import quivr.models.Proof.{And, Digest}

val DoesBridgeClaim: Boolean = true

setUpWallets()

def extractSecret(txId: TransactionId)(targetLockAddr: LockAddress): String = {
  val tx = bifrostQuery.fetchTransaction(txId).unsafeRunSync().get
  val attestation = tx.inputs.map(_.attestation.getPredicate)
    .find(att => targetLockAddr == txBuilder.lockAddress(Lock().withPredicate(att.lock)).unsafeRunSync())
    .get
  // The following is possible because we know the exact structure of the attestation
  val aliceProof = attestation.responses.head.asInstanceOf[And]
  val preimage = aliceProof.right.asInstanceOf[Digest].preimage
  Encoding.encodeToHex(preimage.input.toByteArray ++ preimage.salt.toByteArray)
}

val bridge = Bridge(extractSecret)
val bridgeRpc = BridgeQuery(bridge)
val alice = Alice(bridgeRpc)

alice.initiateRequest()
alice.sendBtcToDesc()

mineBlocks(1)

if(DoesBridgeClaim) {
  alice.notifyBridgeBtcTransfer()
  alice.claimTBtc()
  alice.notifyBridgeTbtcClaim()
} else {
  println("> Alice waiting 1000 blocks...")  // Alice can only reclaim after 1000 blocks
  mineBlocks(1000)
  alice.reclaimBtc()
}
mineBlocks(1)