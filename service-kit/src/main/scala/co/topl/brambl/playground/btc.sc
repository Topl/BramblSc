import co.topl.brambl.playground._
import co.topl.brambl.utils.Encoding
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.currency.BitcoinsInt
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.TransactionInput

import java.security.MessageDigest

setup()
checkBalances()
//handleCall(rpcCli.loadWallet("alice"))
//handleCall(rpcCli.loadWallet("bridge"))

// Alice retrieves her public key and hashes the secret
val secret = "topl-secret".getBytes("UTF-8")
val hash = MessageDigest.getInstance("SHA-256").digest(secret)
val secretHex = Encoding.encodeToHex(secret)
val hashHex = Encoding.encodeToHex(hash)


//val aliceXPrivDesc = handleCall(rpcCli.getDescriptor("alice")).get
//val aliceXPrivKey = extractXPrivKey(aliceXPrivDesc)
//val aliceXPubDesc = handleCall(rpcCli.applyDescriptorChecksum("alice", aliceXPrivDesc)).get("descriptor")
//val aliceXPubKey = extractKey(aliceXPubDesc, '(')

//val aliceXPubDesc = handleCall(rpcCli.getDescriptor("alice", isPrivate = false)).get
//val aliceXPubKey = extractXPubKey(aliceXPubDesc)

// Alice creates a new public key. It does not matter to us how she obtains it, however, it must be a child key (i.e, not contain a range)
val alicePubKey = handleCall(rpcCli.getNewPublicKey("alice")).get
// Alice sends the hash and her public key to the bridge
val aliceReq = Map(
  "hash" -> hashHex,
  "xpub" -> alicePubKey
)

// Bridge creates a descriptor
//val bridgeXPrivDesc = handleCall(rpcCli.getDescriptor("bridge")).get
//val bridgeXPrivKey = extractXPrivKey(bridgeXPrivDesc)


// We retrieve a new public key for the bridge to use. It will also be a child key (no range).
// For this example, the bridge obtains it by generating a new address (thus using a new key), and then retrieving the public key from the address.
val bridgeXPubKey = handleCall(rpcCli.getNewPublicKey("bridge")).get


//val descriptor = s"wsh(andor(pk($bridgeXPrivKey),sha256(${aliceReq("hash")}),pk(${aliceReq("xpub")})))"
//val descriptor = s"wsh(and_v(v:pk($bridgeXPrivKey),sha256($hashHex)))" // this works.. i.e, without alice's

// Bridge creates a descriptor using the hash and Alice's public key and its own public key
val descriptor = s"wsh(andor(pk($bridgeXPubKey),sha256(${aliceReq("hash")}),pk(${aliceReq("xpub")})))"
// Bridge adds the checksum
val desc = handleCall(rpcCli.applyDescriptorChecksum("bridge", descriptor)).get("descriptor")

// Bridge sends back the descriptor to alice.
val bridgeRes = Map(
  "desc" -> desc
)

// Alice creates a new address using the descriptor
val address = handleCall(rpcCli.deriveOneAddresse("alice", bridgeRes("desc"))).get

// Alice sends funds to the address. It does not matter how she obtains the funds or how she sends them.
// For this example, she obtains the funds from block rewards and signs a transaction using her wallet.
val initialFundsUtxo = handleCall(rpcCli.listUnspent("alice")).get.head
val inputs = Vector(TransactionInput.fromTxidAndVout(initialFundsUtxo.txid, UInt32(initialFundsUtxo.vout)))
val outputs = Map(BitcoinAddress(address) -> 49.bitcoins) // 1 BTC as fee
val unprovenTx = handleCall(rpcCli.createRawTransaction(inputs, outputs)).get
val provenTx = handleCall(rpcCli.signRawTransactionWithWallet(unprovenTx, Some("alice"))).get.hex
println("Sending: ")
println(provenTx.outputs.head.scriptPubKey.asm)

val txId = handleCall(rpcCli.sendRawTransaction(provenTx, 0)).get

checkBalances()

mineBlocks(1)
checkBalances()

// Alice sends the transaction id and vout
val aliceNotif = Map(
  "txId" -> txId
)


// Alice Reclaims the funds
val reclaimAddress = handleCall(rpcCli.getNewAddress(Some("alice"))).get
val inputs = Vector(TransactionInput.fromTxidAndVout(aliceNotif("txId"), UInt32(0)))
val outputs = Map(reclaimAddress -> 48.bitcoins) // 1 BTC as fee
val unprovenTx = handleCall(rpcCli.createRawTransaction(inputs, outputs)).get
val provenTx = handleCall(rpcCli.signRawTransactionWithWallet(unprovenTx, Some("alice"))).get.hex
println("Sending: ")
println(provenTx)
val txId = handleCall(rpcCli.sendRawTransaction(provenTx, 0)).get

checkBalances()

mineBlocks(1)
checkBalances()



// look into signrawtransactionwithwallet and the functions in the confluence page; psbt stuff