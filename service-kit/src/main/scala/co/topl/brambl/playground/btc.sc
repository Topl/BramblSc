import co.topl.brambl.playground._
import co.topl.brambl.utils.Encoding
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.crypto.{ExtKey, ExtPrivateKey}
import org.bitcoins.core.currency.BitcoinsInt
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{NonStandardScriptSignature, P2WSHWitnessV0, RawScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint, WitnessTransaction}
import org.bitcoins.core.protocol.{BitcoinAddress, CompactSizeUInt}
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant.{OP_FALSE, ScriptConstant}
import org.bitcoins.core.script.control.{OP_ELSE, OP_ENDIF, OP_NOTIF}
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_SHA256}
import org.bitcoins.core.script.splice.OP_SIZE
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto.{CryptoUtil, HashType, Sign}
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


//val aliceXPrivDesc = handleCall(rpcCli.getDescriptor("alice")).get
//val aliceXPrivKey = extractXPrivKey(aliceXPrivDesc)
//val aliceXPubDesc = handleCall(rpcCli.applyDescriptorChecksum("alice", aliceXPrivDesc)).get("descriptor")
//val aliceXPubKey = extractKey(aliceXPubDesc, '(')

//val aliceXPubDesc = handleCall(rpcCli.getDescriptor("alice", isPrivate = false)).get
//val aliceXPubKey = extractXPubKey(aliceXPubDesc)

// Alice creates a new public key. It does not matter to us how she obtains it, however, it must be a child key (i.e, not contain a range)
//val alicePubKey = handleCall(rpcCli.getNewPublicKey("alice")).get
//val aliceNewAddr = handleCall(rpcCli.getNewAddress(walletNameOpt = Some("alice"))).get
//val aliceAddrInfo = handleCall(rpcCli.getAddressInfo(aliceNewAddr, walletNameOpt = Some("alice"))).get
val aliceRootSecretKeyRaw = (handleCall(rpcCli.listDescriptors("alice", isPrivate = true)).get.head \ "desc").result.get.toString()
val aliceRootSecretKey = aliceRootSecretKeyRaw.substring(aliceRootSecretKeyRaw.indexOf("(") + 1, aliceRootSecretKeyRaw.indexOf("/"))
val alicePath = "m" + aliceRootSecretKeyRaw.substring(aliceRootSecretKeyRaw.indexOf("/"), aliceRootSecretKeyRaw.indexOf(")")).replace("*", "4")
val aliceXPriv = ExtPrivateKey.fromString(aliceRootSecretKey)
val aliceChildPriv = aliceXPriv.deriveChildPrivKey(HDPath.fromString(alicePath))
val alicePubKey = aliceChildPriv.extPublicKey
// SegWitHDPath.fromString("m/84'/0'/0'/0/0")
//val alicePrivKey = SegWitHDPath.fromString(aliceAddrInfo.hdkeypath.get)
// Alice sends the hash and her public key to the bridge
val aliceReq = Map(
  "hash" -> hashHex,
  "xpub" -> ExtKey.toString(alicePubKey)
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

println(txId.hex)

checkBalances()

mineBlocks(1)
checkBalances()

// Alice sends the transaction id and vout
val aliceNotif = TransactionOutPoint(txId, UInt32(0))

// Bridge can verify that the funds have been sent to the address
handleCall(rpcCli.getTxOut(aliceNotif.txId.flip, aliceNotif.vout.toLong)).get

// Only works with our specific descriptor
def descToScriptPubKey(desc: String): RawScriptPubKey = {
  def getInner(in: String): String = {
    val firstP = in.indexOf("(")
    val lastP = in.lastIndexOf(")")
    val x = in.substring(firstP + 1, lastP)
//    println("inner of " + in + ": " + x)
    x
  }
  val inner = {
    val firstStrip = getInner(desc)
    if(firstStrip.startsWith("andor")) getInner(firstStrip)
    else firstStrip
  } split ","
//  println("inner: " + inner)
  val bridgePk = ScriptConstant(getInner(inner(0)))
  val secret = ScriptConstant(getInner(inner(1)))
  // TODO: Not hardcode it
  val alicePk = ScriptConstant(ExtKey.fromString(getInner(inner(2))).hex)
  val size = ScriptConstant("%02x".format(32))

  RawScriptPubKey(Seq(bridgePk, OP_CHECKSIG, OP_NOTIF, alicePk, OP_CHECKSIG, OP_ELSE, OP_SIZE, size, OP_EQUALVERIFY, OP_SHA256, secret, OP_EQUAL, OP_ENDIF))
}

def getAliceSignature(unsignedTx: Transaction, script: RawScriptPubKey, privateKey: Sign): ScriptSignature = {
//  val outPoint = unsignedTx.inputs.head.previousOutput
//  val signingInfo = ECSignatureParams(
//    InputInfo(outPoint = outPoint,
//      output = prevTx.outputs(outPoint.vout.toInt),
//      redeemScriptOpt = Some(script),
//      scriptWitnessOpt = Some(P2WSHWitnessV0(script)),
//      conditionalPath = ConditionalPath.NoCondition),
//    prevTransaction = prevTx,
//    signer = privateKey,
//    hashType = sigHashAll
//  )
//  val partialSig = BitcoinSigner.signSingle(
//    spendingInfo = signingInfo,
//    unsignedTx = unsignedTx,
//    isDummySignature = false)
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
  val signature = privateKey.sign(signableBytes.bytes) // TODO: Append hashtype?
  NonStandardScriptSignature.fromAsm(Seq(ScriptConstant(signature.hex), OP_FALSE))
}

// Alice Reclaims the funds
val reclaimAddress = handleCall(rpcCli.getNewAddress(Some("alice"))).get
val outputs = Map(reclaimAddress -> 48.bitcoins) // 1 BTC as fee
val inputs = Vector(TransactionInput.fromTxidAndVout(aliceNotif.txId.flip, aliceNotif.vout))
val tx = handleCall(rpcCli.createRawTransaction(inputs, outputs)).get
val txWitEmpty = WitnessTransaction.toWitnessTx(tx)
// inside the witness needs to be a raw scriptPubKey, not a P2SHScriptPubKey
// NonStandardScriptPubKey or RawScriptPubKey
val script = descToScriptPubKey(desc)
val aliceSig = getAliceSignature(
  txWitEmpty,
  script,
  aliceChildPriv
)
val txWit = txWitEmpty.updateWitness(0, P2WSHWitnessV0(script, aliceSig))
println("Sending: ")
println(txWit)
val txId = handleCall(rpcCli.sendRawTransaction(txWit, 0)).get

checkBalances()

mineBlocks(1)
checkBalances()



// Following does not work
//val scriptPubKey = handleCall(rpcCli.getAddressInfo(BitcoinAddress(address))).get.scriptPubKey
//val psbt = handleCall(rpcCli.convertToPsbt(txWit)).get
//val signedPsbt = handleCall(rpcCli.walletProcessPSBT(psbt, walletNameOpt = Some("alice"))).get
//val witnessTx = handleCall(rpcCli.signRawTransactionWithWallet(txWit, Some("alice"))).get.hex
//signedPsbt.psbt.transaction
//val finaledPsbt1 = handleCall(rpcCli.finalizePsbt(signedPsbt.psbt)).get
//val finaledPsbt2 = signedPsbt.psbt.finalizePSBT
//val txToSend = signedPsbt.psbt.extractTransaction
//println("Sending: ")
//println(txToSend)
//val txId = handleCall(rpcCli.sendRawTransaction(txToSend, 0)).get