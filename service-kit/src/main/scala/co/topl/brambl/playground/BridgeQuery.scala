package co.topl.brambl.playground

import cats.effect.unsafe.implicits.global
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.{Indices, LockAddress, TransactionId, TransactionOutputAddress}
import co.topl.brambl.playground.BridgeQuery.{BridgeRequest, BridgeResponse}
import co.topl.brambl.playground.ScriptBuilder.{PegIn, PegOut}
import co.topl.brambl.utils.Encoding
import org.bitcoins.crypto.DoubleSha256DigestBE
import quivr.models.VerificationKey

case class BridgeQuery(bridge: Bridge) {

  def initiateRequest(request: BridgeRequest, isPegIn: Boolean): BridgeResponse = {
    print("\n============================" + "Bridge Receives Request" + "============================\n")
    val scriptBuilder: ScriptBuilder = if (isPegIn) PegIn else PegOut
    println("> Bridge generating keypair...")
    val z = if (isPegIn) 5 else 6
    println(s"> Hardcoding indices to be 5'/5'/$z")
    val idx = Indices(5, 5, z)
    val toplVk = bridge.getChildVk(idx)
    val btcKey = bridge.getChildSecretKey(idx)
    println("> Bridge generating descriptor...")
    val desc = scriptBuilder.generateDescriptor(btcKey.hex, request.hash, request.bitcoinPk)
    println("> watcher importing descriptor...")
    val importDescSuccessful = handleCall(rpcCli.importDescriptor(bridge.watcherName, desc)).get
    println("> watcher importing descriptor successful: " + importDescSuccessful)
    val toplLock = scriptBuilder.generateToplLock(request.toplVk, request.hash, toplVk)
    val toplAddr = txBuilder.lockAddress(toplLock).unsafeRunSync()
    bridge.walletStateApi
      .updateWalletState(
        Encoding.encodeToBase58Check(toplLock.getPredicate.toByteArray),
        toplAddr.toBase58(),
        Some("ExtendedEd25519"),
        Some(Encoding.encodeToBase58(toplVk.toByteArray)),
        idx
      )
      .unsafeRunSync()
    BridgeResponse(desc, toplLock, toplAddr)
  }

  def notifyOfBtcTransfer(txOut: String): LockAddress =
    bridge.triggerMinting(txOut)

  def notifyOfTbtcClaim(txId: TransactionId, desc: String): Unit =
    bridge.claimBtc(txId, desc)

  def notifyOfTbtcTransfer(utxoId: TransactionOutputAddress): String =
    bridge.triggerBtcTransfer(utxoId)

  def notifyOfBtcClaim(txId: DoubleSha256DigestBE, desc: String): Unit =
    bridge.claimTbtc(txId, desc)
}

object BridgeQuery {

  case class BridgeRequest(
    hash:      String,
    bitcoinPk: String,
    toplVk:    VerificationKey
  )

  case class BridgeResponse(
    desc:        String,
    toplLock:    Lock,
    toplAddress: LockAddress // Serves as a checksum for the toplLock
  )
}
