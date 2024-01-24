import co.topl.brambl.playground._
import co.topl.brambl.utils.Encoding
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.crypto.ExtPrivateKey
import org.bitcoins.core.currency.BitcoinsInt
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{NonStandardScriptSignature, P2WSHWitnessV0, RawScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint, WitnessTransaction}
import org.bitcoins.core.protocol.{BitcoinAddress, CompactSizeUInt}
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant.{OP_0, ScriptConstant}
import org.bitcoins.core.script.control.{OP_ELSE, OP_ENDIF, OP_NOTIF}
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_SHA256}
import org.bitcoins.core.script.splice.OP_SIZE
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import java.security.MessageDigest

setup()
checkBalances()
//handleCall(rpcCli.loadWallet("alice"))
//handleCall(rpcCli.loadWallet("bridge"))

// Alice retrieves her public key and hashes the secret
// secret has to be 32 bytes
val secret = "topl-secret".getBytes("UTF-8")
val hash = MessageDigest.getInstance("SHA-256").digest(secret)
val secretHex = Encoding.encodeToHex(secret)
val hashHex = Encoding.encodeToHex(hash)


// Alice creates a new public key. It does not matter to us how she obtains it, however, it must be a child key (i.e, not contain a range)
val aliceRootSecretKeyRaw = (handleCall(rpcCli.listDescriptors("alice", isPrivate = true)).get.head \ "desc").result.get.toString()
val aliceRootSecretKey = aliceRootSecretKeyRaw.substring(aliceRootSecretKeyRaw.indexOf("(") + 1, aliceRootSecretKeyRaw.indexOf("/"))
val alicePath = "m" + aliceRootSecretKeyRaw.substring(aliceRootSecretKeyRaw.indexOf("/"), aliceRootSecretKeyRaw.indexOf(")")).replace("*", "4")
val aliceXPriv = ExtPrivateKey.fromString(aliceRootSecretKey)
val aliceChildPriv = aliceXPriv.deriveChildPrivKey(HDPath.fromString(alicePath))
val alicePubKey = aliceChildPriv.extPublicKey.key
// SegWitHDPath.fromString("m/84'/0'/0'/0/0")
// Alice sends the hash and her public key to the bridge
val aliceReq = Map(
  "hash" -> hashHex,
  "xpub" -> alicePubKey.hex
)

// We retrieve a new public key for the bridge to use. It will also be a child key (no range).
val bridgeRootSecretKeyRaw = (handleCall(rpcCli.listDescriptors("bridge", isPrivate = true)).get.head \ "desc").result.get.toString()
val bridgeRootSecretKey = bridgeRootSecretKeyRaw.substring(bridgeRootSecretKeyRaw.indexOf("(") + 1, bridgeRootSecretKeyRaw.indexOf("/"))
val bridgePath = "m" + bridgeRootSecretKeyRaw.substring(bridgeRootSecretKeyRaw.indexOf("/"), bridgeRootSecretKeyRaw.indexOf(")")).replace("*", "4")
val bridgeXPriv = ExtPrivateKey.fromString(bridgeRootSecretKey)
val bridgeChildPriv = bridgeXPriv.deriveChildPrivKey(HDPath.fromString(bridgePath))
val bridgePubKey = bridgeChildPriv.extPublicKey.key.hex


// Bridge creates a descriptor using the hash and Alice's public key and its own public key
val descriptor = s"wsh(andor(pk($bridgePubKey),sha256(${aliceReq("hash")}),pk(${aliceReq("xpub")})))"
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
// 20 byte witness program (P2WPKH)
println("Witness Program from Address: " + handleCall(rpcCli.getAddrWitnessProgran("alice", initialFundsUtxo.address.get.value)))
println("Witness Program in our TX: " + CryptoUtil.sha256Hash160(provenTx.asInstanceOf[WitnessTransaction].witness.witnesses.head.stack.head))
println("======")

val txId = handleCall(rpcCli.sendRawTransaction(provenTx, 0)).get

println(txId.hex)

mineBlocks(1)
checkBalances()

// Alice sends the transaction id and vout
val aliceNotif = TransactionOutPoint(txId, UInt32(0))

// Bridge can verify that the funds have been sent to the address
val complexUtxo = handleCall(rpcCli.getTxOut(aliceNotif.txId.flip, aliceNotif.vout.toLong)).get

def sizeOf(toPush: String): ScriptConstant = {
  val size = toPush.length / 2
  ScriptConstant("%02x".format(size))
}
// Only works with our specific descriptor
// We only support 33byte public keys in hex
def descToScriptPubKey(desc: String): RawScriptPubKey = {
  def getInner(in: String): String = {
    val firstP = in.indexOf("(")
    val lastP = in.lastIndexOf(")")
    in.substring(firstP + 1, lastP)
  }
  val inner = {
    val firstStrip = getInner(desc)
    if(firstStrip.startsWith("andor")) getInner(firstStrip)
    else firstStrip
  } split ","
  val bridgePk = ScriptConstant(getInner(inner(0)))
  val secret = ScriptConstant(getInner(inner(1)))
  val alicePk = ScriptConstant(getInner(inner(2)))
  val size = ScriptConstant("%02x".format(32))
  val scriptTokens = Seq(
    sizeOf(bridgePk.hex), // op codes 1-75 indicates the number of bytes to push
    bridgePk,
    OP_CHECKSIG,
    OP_NOTIF,
    sizeOf(alicePk.hex),
    alicePk,
    OP_CHECKSIG,
    OP_ELSE,
    OP_SIZE,
    sizeOf(size.hex),
    size,
    OP_EQUALVERIFY,
    OP_SHA256,
    sizeOf(secret.hex),
    secret,
    OP_EQUAL,
    OP_ENDIF
  )

  println("script: " + scriptTokens.map(_.hex).mkString(""))
  RawScriptPubKey(scriptTokens)
}

def getAliceSignature(unsignedTx: Transaction, script: RawScriptPubKey, privateKey: Sign): ScriptSignature = {
  /** BIP-143
   * Double SHA256 of the serialization of:
   * 1. nVersion of the transaction (4-byte little endian)
   * 2. hashPrevouts (32-byte hash)
   * 3. hashSequence (32-byte hash)
   * 4. outpoint (32-byte hash + 4-byte little endian)
   * 5. scriptCode of the input (serialized as scripts inside CTxOuts)
   * 6. value of the output spent by this input (8-byte little endian)
   * 7. nSequence of the input (4-byte little endian)
   * 8. hashOutputs (32-byte hash)
   * 9. nLocktime of the transaction (4-byte little endian)
   * 10. sighash type of the signature (4-byte little endian)
   *
   * We are assuming hashtype is hashAll
   *
   * We are also using TransactionSignatureSerializer's implementation from bitcoin-s as inspiration
   */
  val scriptBytes = BytesUtil.toByteVector(script.asm)
  val serializedTxForSignature =
    unsignedTx.version.bytesLE ++
      CryptoUtil.doubleSHA256(BytesUtil.toByteVector(unsignedTx.inputs.map(_.previousOutput))).bytes ++
      CryptoUtil.doubleSHA256(unsignedTx.inputs.map(_.sequence).foldLeft(ByteVector.empty)(_ ++ _.bytesLE)).bytes ++
      unsignedTx.inputs.head.previousOutput.bytes ++
      CompactSizeUInt.calc(scriptBytes).bytes ++ scriptBytes ++
      unsignedTx.outputs.head.value.bytesLE ++
      unsignedTx.inputs.head.sequence.bytesLE ++
      CryptoUtil.doubleSHA256(BytesUtil.toByteVector(unsignedTx.outputs)).bytes ++
      unsignedTx.lockTime.bytesLE ++
      Int32(HashType.sigHashAll.num).bytesLE

  val signableBytes = CryptoUtil.doubleSHA256(serializedTxForSignature)
  val signature = privateKey.sign(signableBytes.bytes)
  // append 1 byte hash type onto the end, per BIP-066
  val sig = ECDigitalSignature(signature.bytes ++ ByteVector.fromByte(HashType.sigHashAll.byte))
  require(
    sig.isStrictEncoded,
    "We did not create a signature that is strictly encoded, got: " + sig)
  require(DERSignatureUtil.isLowS(sig), "Sig does not have a low s value")
  NonStandardScriptSignature.fromAsm(Seq(
    OP_0,
    sizeOf(sig.hex),
    ScriptConstant(sig.hex)
  ))
}

// Alice Reclaims the funds
val reclaimAddress = handleCall(rpcCli.getNewAddress(Some("alice"))).get
val outputs = Map(reclaimAddress -> 48.bitcoins) // 1 BTC as fee
val inputs = Vector(TransactionInput.fromTxidAndVout(aliceNotif.txId.flip, aliceNotif.vout))
val tx = handleCall(rpcCli.createRawTransaction(inputs, outputs)).get
val txWitEmpty = WitnessTransaction.toWitnessTx(tx)
// inside the witness needs to be a raw scriptPubKey, not a P2SHScriptPubKey
// NonStandardScriptPubKey or RawScriptPubKey
val scriptInner = descToScriptPubKey(desc)
val aliceSig = getAliceSignature(
  txWitEmpty,
  scriptInner,
  aliceChildPriv
)
val witness = P2WSHWitnessV0(scriptInner, aliceSig)
val txWit = txWitEmpty.updateWitness(0, witness)
println("Sending: ")
println(txWit)
// 32 byte witness program (P2WSH)
println("script in our TX: " + txWit.witness.witnesses.head.stack.head.toHex)

println("Witness Program from Address: \n" + handleCall(rpcCli.getAddrWitnessProgran("alice", address)))
println("hash of our redeem script: \n" + CryptoUtil.sha256(scriptInner.asmBytes))
println("======")
val txId = handleCall(rpcCli.sendRawTransaction(txWit, 0)).get

mineBlocks(1)
checkBalances()

