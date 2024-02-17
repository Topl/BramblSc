package co.topl.brambl.playground

import cats.Id
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.{LockAddress, TransactionId, TransactionOutputAddress}
import co.topl.brambl.playground.ScriptBuilder.{PegIn, PegOut}
import co.topl.brambl.playground.monitoring.Models.{BridgeRequest, BridgeResponse}
import co.topl.brambl.utils.Encoding
import co.topl.quivr.api.Proposer
import com.google.protobuf.ByteString
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.{TransactionOutPoint, WitnessTransaction}
import org.bitcoins.crypto.DoubleSha256DigestBE
import quivr.models.{Digest, Preimage, VerificationKey}

import java.io.{BufferedReader, InputStreamReader}
import java.net.{HttpURLConnection, URL}
import scala.concurrent.duration.DurationInt

case class User(walletName: String) {
  val toplWallet = new ToplWallet(walletName)

  val btcWallet = new BitcoinWallet(walletName) {

    override def initBtcFunds(): Unit = {
      println(s"Mining 101 blocks to $walletName's wallet...")
      mineBlocks(1, walletName)
      mineBlocks(100)
    }
  }

  def init(): Unit =
    toplWallet.initToplFunds()

  init()

  def initiateRequest(isPegIn: Boolean): BridgeResponse = {
    println(s"> $walletName generating 32 byte secret...")
    val secrets = generateSecret()
    println(s"> $walletName saving Preimage and Digest pair to their wallet state...")
    val preimage = Preimage(ByteString.copyFrom(secrets("secret")), ByteString.copyFrom(secrets("salt")))
    val digest = Digest(ByteString.copyFrom(secrets("hash")))
    val digestProposition = Proposer.digestProposer[Id].propose(("Sha256", digest))
    toplWallet.walletStateApi.addPreimage(preimage, digestProposition.getDigest).unsafeRunSync()
    val idx = toplWallet.walletStateApi.getNextIndicesForFunds("self", "default").unsafeRunSync().get
    val toplVk = toplWallet.getChildVk(idx)
    val btcKey = btcWallet.getChildSecretKey(idx)
    println(s"> $walletName sending hash and public key to bridge...")
    val bridgeReq = BridgeRequest(Encoding.encodeToHex(secrets("hash")), btcKey.publicKey.hex, toplVk)
    val resp = bridgeRequest(bridgeReq, isPegIn)
    println("> watcher importing descriptor...")
    val importDescSuccessful = handleCall(rpcCli.importDescriptor(btcWallet.watcherName, resp.desc)).get
    println("> watcher importing descriptor successful: " + importDescSuccessful)
    println(s"> $walletName storing descriptor and (toplVk, toplLock, toplIdx) in their wallet state...")
    btcWallet.addWalletEntry(idx, resp.desc, resp.toplAddress)
    toplWallet.walletStateApi
      .updateWalletState(
        Encoding.encodeToBase58Check(resp.toplLock.getPredicate.toByteArray),
        resp.toplAddress.toBase58(),
        Some("ExtendedEd25519"),
        Some(Encoding.encodeToBase58(toplVk.toByteArray)),
        idx
      )
      .unsafeRunSync()
    resp
  }

  def bridgeRequest(req: BridgeRequest, isPegIn: Boolean): BridgeResponse = {
    val resp = doRequest(if (isPegIn) "pegin" else "pegout", req.toMap)
    BridgeResponse.fromJson(resp)
  }

  def doRequest(path: String, params: Map[String, String]): String = {
    val url = new URL(s"http://localhost:1997/$path?${params.map { case (k, v) => s"$k=$v" }.mkString("&")}")
    val con: HttpURLConnection  = url.openConnection.asInstanceOf[HttpURLConnection]
    con.setRequestMethod("GET")
    con.setDoOutput(true)
    val in = new BufferedReader(new InputStreamReader(con.getInputStream))
    val resp = new StringBuffer()
    var inputLine = in.readLine()
    while(inputLine != null){
      resp.append(inputLine)
      inputLine = in.readLine()
    }
    in.close()
    con.disconnect()
    resp.toString
  }

  def notifyBridgeOfTbtcClaim(txId: TransactionId, addr: LockAddress): Unit = {
    doRequest("notifyOfTbtcClaim", Map("txId" -> Encoding.encodeToHex(txId.toByteArray), "addr" -> addr.toBase58()))
  }

  def initiatePegIn(): BridgeResponse = {
    print("\n============================" + s"$walletName initiates Peg-In" + "============================\n")
    initiateRequest(true)
  }

  def initiatePegOut(): BridgeResponse = {
    print("\n============================" + s"$walletName initiates Peg-Out" + "============================\n")
    initiateRequest(false)
  }
  def sendBtcToDesc(desc: String): Unit = {
    val txOut = btcWallet.sendBtcToDesc(desc)
    btcWallet.addDescTxOutEntry(desc, txOut)
    displayBalance()
  }

  def sendTbtcToAddress(lock: Lock): TransactionOutputAddress = {
    print("\n============================" + s"$walletName sends TBTC to Lock" + "============================\n")
    val sendTbtc = for {
      // This is where alice sent the TBTC
      inLock <- toplWallet.walletStateApi.getLock("self", "default", 2).map(_.get.getPredicate)
      inAddr <- txBuilder.lockAddress(Lock().withPredicate(inLock))
      // Only TBTC should be present at this address
      txos       <- genusQueryApi.queryUtxo(inAddr)
      outputAddr <- txBuilder.lockAddress(lock)
      unprovenTx <- txBuilder.buildTransferAllTransaction(
        txos,
        inLock,
        outputAddr,
        inAddr, // trivial
        0 // TODO: Fees?
      )
      provenTx <- toplWallet.credentialler.prove(unprovenTx.toOption.get)
      txId     <- bifrostQuery.broadcastTransaction(provenTx)
    } yield (txId, outputAddr)
    val txId = sendTbtc.unsafeRunSync()
    Thread.sleep(15000)
    val tbtcBalance = toplWallet.getTbtcBalance(txId._2)
    println(s"$walletName transferred $tbtcBalance tBTC (unclaimed)")
    displayBalance()
    TransactionOutputAddress(PRIVATE_NETWORK_ID, MAIN_LEDGER_ID, 0, txId._1)
  }

  def claimBtc(desc: String): DoubleSha256DigestBE = {
    print("\n============================" + s"$walletName claims BTC" + "============================\n")
    val expectedAddress = BitcoinAddress(handleCall(rpcCli.deriveOneAddress(btcWallet.walletName, desc)).get)
    val utxo = handleCall(rpcCli.listUnspent(btcWallet.watcherName)).get.filter(_.address.get == expectedAddress).head
    println(s"> $walletName creating unproven TX...")
    val tx = btcWallet.createToWalletTx(TransactionOutPoint(utxo.txid, UInt32(utxo.vout)))
    println(s"> $walletName deriving witnessScript...")
    val scriptInner = PegOut.descToScriptPubKey(desc)
    println(s"> $walletName derives script signature...")
    val idx = btcWallet.getIndicesByDesc(desc)
    val secret = (for {
      lock <- toplWallet.walletStateApi.getLockByIndex(idx)
      digestProp = lock.get.challenges.head.getRevealed.getAnd.right.getDigest
      preimage <- toplWallet.walletStateApi.getPreimage(digestProp).map(_.get)
    } yield Encoding.encodeToHex(preimage.input.toByteArray ++ preimage.salt.toByteArray)).unsafeRunSync()
    val userSig = SignatureBuilder.PegOut.getClaimSig(
      getTxSignature(tx, scriptInner, btcWallet.getChildSecretKey(idx).hex),
      secret
    )
    println(s"> $walletName adds the witness to the TX...")
    val txWit = WitnessTransaction.toWitnessTx(tx).updateWitness(0, P2WSHWitnessV0(scriptInner, userSig))
    println(s"> $walletName submits TX...")
    val txId = handleCall(rpcCli.sendRawTransaction(txWit, 0)).get
    mineBlocks(1)
    displayBalance()
    txId
  }

  def reclaimBtc(desc: String): Unit = {
    print("\n============================" + s"$walletName reclaims BTC" + "============================\n")
    val utxoToSpend = TransactionOutPoint.fromString(btcWallet.getTxOut(desc))
    println(s"> $walletName creating unproven TX...")
    val tx = btcWallet.createToWalletTx(utxoToSpend, spendTimeLock = true)
    println(s"> $walletName deriving witnessScript...")
    val scriptInner = PegIn.descToScriptPubKey(desc)
    println(s"> $walletName derives script signature...")
    val sk = btcWallet.getChildSecretKey(btcWallet.getIndicesByDesc(desc))
    val aliceSig = SignatureBuilder.PegIn.getReclaimSig(getTxSignature(tx, scriptInner, sk.hex))
    println(s"> $walletName adds the witness to the TX...")
    val txWit = WitnessTransaction.toWitnessTx(tx).updateWitness(0, P2WSHWitnessV0(scriptInner, aliceSig))
    println(s"> $walletName submits TX...")
    val spendingTx = handleCall(rpcCli.getTxOut(utxoToSpend.txIdBE, utxoToSpend.vout.toLong), debug = true).get
    println(s"number of confirmations: ${spendingTx.confirmations}")
    handleCall(rpcCli.sendRawTransaction(txWit, 0), debug = true).get
    mineBlocks(1)
    displayBalance()
  }

  def claimTBtc(inputAddress: LockAddress): TransactionId = {
    println("\n============================" + s"$walletName claims tBTC" + "============================\n")
    val claimAsset = for {
      inputLock <- toplWallet.walletStateApi.getLockByAddress(inputAddress.toBase58()).map(_.get)
      txos      <- genusQueryApi.queryUtxo(inputAddress)
      claimIdx
        <- toplWallet.walletStateApi.getNextIndicesForFunds("self", "default").map(_.get)
      claimLock <- toplWallet.walletStateApi.getLock("self", "default", claimIdx.z)
      claimAddr <- txBuilder.lockAddress(claimLock.get)
      claimVk <- toplWallet.walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
        toplWallet.walletApi.deriveChildVerificationKey(
          VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get),
          claimIdx.z
        )
      }
      _ <- toplWallet.walletStateApi.updateWalletState(
        Encoding.encodeToBase58Check(claimLock.get.getPredicate.toByteArray),
        claimAddr.toBase58(),
        Some("ExtendedEd25519"),
        Some(Encoding.encodeToBase58(claimVk.toByteArray)),
        claimIdx
      )
      unprovenTx <- txBuilder.buildTransferAllTransaction(
        txos,
        inputLock,
        claimAddr,
        claimAddr,
        0L // TODO: Fee should be able to be a non LVL type
      )
      provenTx <- toplWallet.credentialler.prove(unprovenTx.toOption.get)
      txId     <- bifrostQuery.broadcastTransaction(provenTx)
    } yield (txId, claimAddr)
    val txId = claimAsset.unsafeRunSync()
    println("waiting 20 seconds for the transaction to be processed")
    IO.unit.andWait(20.seconds).unsafeRunSync()
    println("getting balance")
    val tbtcBalance = toplWallet.getTbtcBalance(txId._2)
    println(s"$walletName owns $tbtcBalance tBTC (claimed)")
    displayBalance()
    txId._1
  }

  def displayBalance(): Unit = {
    val balance = Seq(
      toplWallet.getBalance(),
      btcWallet.getBalance()
    ) mkString("=====================================", "=====================================", "=====================================")
    println(balance)
  }
}
