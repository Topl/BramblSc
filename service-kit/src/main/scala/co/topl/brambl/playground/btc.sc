import co.topl.brambl.playground._
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto._

val DoesBridgeClaim: Boolean = false

println("> Setting up wallets...")
handleCall(rpcCli.createWallet("dummy", descriptors = true))
handleCall(rpcCli.createWallet("alice", descriptors = true))
handleCall(rpcCli.createWallet("bridge", descriptors = true))
mineBlocks(1, "alice")
mineBlocks(100)
checkBalances()

// Mapping of information that each participant has access to
var aliceCtx: Map[String, String] = Map()
var bridgeCtx: Map[String, String] = Map()

print("\n============================" + "Alice initiates request" + "============================\n")
println("> Alice generating 32 byte secret...")
aliceCtx ++= generateSecret()
println("> Alice generating keypair...")
aliceCtx ++= getChildKeyPair("alice")
println("> Alice sending hash and public key to bridge...")
bridgeCtx ++= Map("hash" -> aliceCtx("hash"), "vkAlice" -> aliceCtx("vk"))

print("\n============================" + "Bridge builds Descriptor" + "============================\n")
println("> Bridge generating keypair...")
bridgeCtx ++= getChildKeyPair("bridge")
println("> Bridge generating descriptor...")
bridgeCtx += ("desc" -> generateDescriptor(bridgeCtx("vk"), bridgeCtx("hash"), bridgeCtx("vkAlice")))
println("> Bridge generating descriptor address...")
bridgeCtx += ("address" -> handleCall(rpcCli.deriveOneAddress("bridge", bridgeCtx("desc"))).get)
println("> Bridge sending descriptor to Alice...")
aliceCtx += ("desc" -> bridgeCtx("desc"))

print("\n============================" + "Alice sends BTC to Descriptor" + "============================\n")
println("> Alice deriving address from descriptor...")
aliceCtx += ("address" -> handleCall(rpcCli.deriveOneAddress("alice", aliceCtx("desc"))).get)
println("> Alice sends BTC to address...")
aliceCtx += ("txOut" -> sendFromWallet("alice", aliceCtx("address")).toHumanReadableString)
println("> Alice sends txOut to bridge...")
bridgeCtx += ("txOut" -> aliceCtx("txOut"))
println(aliceCtx("txOut"))

mineBlocks(1)
checkBalances()

if(DoesBridgeClaim) {
  print("\n============================" + "Bridge claims BTC" + "============================\n")
  val utxoToSpend = TransactionOutPoint.fromString(bridgeCtx("txOut"))
  println("> Bridge verifies funds...")
  verifyTxOutAndGetAmount(utxoToSpend, bridgeCtx("address"))
  println("> Bridge creating unproven TX...")
  val tx = createToWalletTx("bridge", utxoToSpend)
  println("> Bridge deriving witnessScript...")
  val scriptInner = descToScriptPubKey(bridgeCtx("desc"))
  println("> Bridge verifies validity of witnessScript...")
  verifyWitnessScript("bridge", scriptInner, bridgeCtx("address"))
  println("> Bridge derives script signature...")
  val bridgeSig = ScriptSignatures.getBridgeClaimSig(
    getTxSignature(tx, scriptInner, bridgeCtx("sk")),
    aliceCtx("secret") // In reality, this would be retrieved from a Topl transaction
  )
  println("> Bridge adds the witness to the TX...")
  val txWit = WitnessTransaction.toWitnessTx(tx).updateWitness(0, P2WSHWitnessV0(scriptInner, bridgeSig))
  println("> Bridge submits TX...")
  handleCall(rpcCli.sendRawTransaction(txWit, 0)).get
} else {
  print("\n============================" + "Alice reclaims BTC" + "============================\n")
  // Alice can only reclaim after 1000 blocks
  println("> Alice waiting 1000 blocks...")
  mineBlocks(1000)
  checkBalances()
  val utxoToSpend = TransactionOutPoint.fromString(aliceCtx("txOut"))
  println("> Alice verifies funds...")
  verifyTxOutAndGetAmount(utxoToSpend, aliceCtx("address"))
  println("> Alice creating unproven TX...")
  val tx = createToWalletTx("alice", utxoToSpend, spendTimeLock = true)
  println(tx)
  println("> Alice deriving witnessScript...")
  val scriptInner = descToScriptPubKey(aliceCtx("desc"))
  println("> Alice verifies validity of witnessScript...")
  verifyWitnessScript("alice", scriptInner, aliceCtx("address"))
  println("> Alice derives script signature...")
  val aliceSig = ScriptSignatures.getUserReclaimSig(getTxSignature(tx, scriptInner, aliceCtx("sk")))
  println("> Alice adds the witness to the TX...")
  val txWit = WitnessTransaction.toWitnessTx(tx).updateWitness(0, P2WSHWitnessV0(scriptInner, aliceSig))
  println("> Alice submits TX...")
  handleCall(rpcCli.sendRawTransaction(txWit, 0), debug = true).get
}
mineBlocks(1)
checkBalances()