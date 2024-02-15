package co.topl.brambl.playground

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.{Indices, LockAddress, TransactionId, TransactionOutputAddress}
import co.topl.brambl.playground.BridgeQuery.{BridgeRequest, BridgeResponse}
import co.topl.brambl.playground.ScriptBuilder.{PegIn, PegOut}
import co.topl.brambl.playground.monitoring.MonitoringService.ToMonitor
import co.topl.brambl.utils.Encoding
import org.bitcoins.crypto.DoubleSha256DigestBE
import quivr.models.VerificationKey

case class BridgeQuery(bridge: Bridge, pegInLockAddrs: ToMonitor[IO, LockAddress], pegInDescs: ToMonitor[IO, String]) {

  def initiateRequest(request: BridgeRequest, isPegIn: Boolean): BridgeResponse = {
    print("\n============================" + "Bridge Receives Request" + "============================\n")
    val scriptBuilder: ScriptBuilder = if (isPegIn) PegIn else PegOut
    println("> Bridge generating keypair...")
    val z = if (isPegIn) 5 else 6
    println(s"> Hardcoding indices to be 5'/5'/$z")
    val idx = Indices(5, 5, z)
    val toplVk = bridge.toplWallet.getChildVk(idx)
    val btcKey = bridge.btcWallet.getChildSecretKey(idx)
    println("> Bridge generating descriptor...")
    val desc = scriptBuilder.generateDescriptor(btcKey.publicKey.hex, request.hash, request.bitcoinPk)
    println("> watcher importing descriptor...")
    val importDescSuccessful = handleCall(rpcCli.importDescriptor(bridge.btcWallet.watcherName, desc)).get
    println("> watcher importing descriptor successful: " + importDescSuccessful)
    val toplLock = scriptBuilder.generateToplLock(request.toplVk, request.hash, toplVk)
    val toplAddr = txBuilder.lockAddress(toplLock).unsafeRunSync()
    bridge.btcWallet.addWalletEntry(idx, desc, toplAddr)
    bridge.toplWallet.walletStateApi
      .updateWalletState(
        Encoding.encodeToBase58Check(toplLock.getPredicate.toByteArray),
        toplAddr.toBase58(),
        Some("ExtendedEd25519"),
        Some(Encoding.encodeToBase58(toplVk.toByteArray)),
        idx
      )
      .unsafeRunSync()
    if (isPegIn) {
      println("> Adding peg-in addresses to monitor...")
      pegInLockAddrs.add(toplAddr).unsafeRunSync()
      pegInDescs.add(desc).unsafeRunSync()
    } else {}
    BridgeResponse(desc, toplLock, toplAddr)
  }

  def notifyOfBtcTransfer(txOut: String, desc: String): LockAddress =
    bridge.triggerMinting(txOut, desc)

  def notifyOfTbtcClaim(txId: TransactionId, addr: LockAddress): Unit =
    bridge.claimBtc(txId, addr)

  def notifyOfTbtcTransfer(utxoId: TransactionOutputAddress): String =
    bridge.triggerBtcTransfer(utxoId)

  def notifyOfBtcClaim(txId: DoubleSha256DigestBE, desc: String): Unit =
    bridge.claimTbtc(txId, desc)
  def notifyOfBtcReclaim(desc: String): Unit =
    bridge.reclaimTbtc(desc)
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
