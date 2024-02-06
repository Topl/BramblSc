package co.topl.brambl.playground

import cats.Id
import cats.effect.unsafe.implicits.global
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.{Indices, LockAddress, TransactionId, TransactionOutputAddress}
import co.topl.brambl.playground.BridgeQuery.BridgeRequest
import co.topl.brambl.playground.ScriptBuilder.PegIn
import co.topl.brambl.utils.Encoding
import co.topl.quivr.api.Proposer
import com.google.protobuf.ByteString
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.{TransactionOutPoint, WitnessTransaction}
import org.bitcoins.crypto.DoubleSha256DigestBE
import quivr.models.{Digest, Preimage, VerificationKey}

case class Alice(bridgeRpc: BridgeQuery) extends ToplWallet with BitcoinWallet {
  override val walletName: String = "alice"

  override def initBtcFunds(): Unit = {
    println("Mining 100 blocks to Alice's wallet...")
    mineBlocks(1, walletName)
    mineBlocks(100)
  }

  def initiateRequest(isPegIn: Boolean): (String, Lock) = {
    println("> Alice generating 32 byte secret...")
    val secrets = generateSecret()
    println("> Alice saving Preimage and Digest pair to her wallet state...")
    val preimage = Preimage(ByteString.copyFrom(secrets("secret")), ByteString.copyFrom(secrets("salt")))
    val digest = Digest(ByteString.copyFrom(secrets("hash")))
    val digestProposition = Proposer.digestProposer[Id].propose(("Sha256", digest))
    walletStateApi.addPreimage(preimage, digestProposition.getDigest).unsafeRunSync()
    val z = if (isPegIn) 5 else 6
    println(s"> Hardcoding indices to be 5'/5'/$z")
    val idx = Indices(5, 5, z)
    val toplVk = getChildVk(idx)
    val btcKey = getChildSecretKey(idx)
    println("> Alice sending hash and public key to bridge...")
    val bridgeReq = BridgeRequest(Encoding.encodeToHex(secrets("hash")), btcKey.publicKey.hex, toplVk)
    val resp = bridgeRpc.initiateRequest(bridgeReq, isPegIn)
    println("> Alice storing descriptor and (toplVk, toplLock, toplIdx) in her wallet state...")
    addWalletEntry(idx, resp.desc)
    walletStateApi
      .updateWalletState(
        Encoding.encodeToBase58Check(resp.toplLock.getPredicate.toByteArray),
        resp.toplAddress.toBase58(),
        Some("ExtendedEd25519"),
        Some(Encoding.encodeToBase58(toplVk.toByteArray)),
        idx
      )
      .unsafeRunSync()
    (resp.desc, resp.toplLock)
  }

  def initiatePegIn(): String = {
    print("\n============================" + "Alice initiates Peg-In" + "============================\n")
    initiateRequest(true)._1
  }

  def initiatePegOut(): Lock = {
    print("\n============================" + "Alice initiates Peg-Out" + "============================\n")
    initiateRequest(false)._2
  }

  def sendTbtcToAddress(lock: Lock): TransactionOutputAddress = {
    print("\n============================" + "Alice sends TBTC to Lock" + "============================\n")
    val sendTbtc = for {
      // Hardcoding indices to be 5'/5'/5, since Alice will need to use the same indices as the ones used in the peg-in
      inLock <- walletStateApi.getLockByIndex(Indices(5, 5, 5)).map(_.get)
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
      provenTx <- credentialler.prove(unprovenTx.toOption.get)
      txId     <- bifrostQuery.broadcastTransaction(provenTx)
    } yield (txId, outputAddr)
    val txId = sendTbtc.unsafeRunSync()
    Thread.sleep(15000)
    val tbtcBalance = getTbtcBalance(txId._2)
    println(s"Alice transferred $tbtcBalance tBTC (unclaimed)")
    TransactionOutputAddress(PRIVATE_NETWORK_ID, MAIN_LEDGER_ID, 0, txId._1)
  }

  def claimBtc(desc: String): DoubleSha256DigestBE = {
    print("\n============================" + "Alice claims BTC" + "============================\n")
    val utxoToSpend = TransactionOutPoint.fromString(getTxOut(desc))
    println("> Alice creating unproven TX...")
    val tx = createToWalletTx(utxoToSpend)
    println("> Alice deriving witnessScript...")
    val scriptInner = PegIn.descToScriptPubKey(desc)
    println("> Alice derives script signature...")
    val idx = getIndicesByDesc(desc)
    val secret = (for {
      lock <- walletStateApi.getLockByIndex(idx)
      digestProp = lock.get.challenges.head.getRevealed.getAnd.right.getDigest
      preimage <- walletStateApi.getPreimage(digestProp).map(_.get)
    } yield Encoding.encodeToHex(preimage.input.toByteArray ++ preimage.salt.toByteArray)).unsafeRunSync()
    val userSig = SignatureBuilder.PegOut.getClaimSig(
      getTxSignature(tx, scriptInner, getChildSecretKey(idx).hex),
      secret
    )
    println("> Alice adds the witness to the TX...")
    val txWit = WitnessTransaction.toWitnessTx(tx).updateWitness(0, P2WSHWitnessV0(scriptInner, userSig))
    println("> Alice submits TX...")
    val txId = handleCall(rpcCli.sendRawTransaction(txWit, 0)).get
    mineBlocks(1)
    txId
  }

  def reclaimBtc(desc: String): Unit = {
    print("\n============================" + "Alice reclaims BTC" + "============================\n")
    val utxoToSpend = TransactionOutPoint.fromString(getTxOut(desc))
    println("> Alice creating unproven TX...")
    val tx = createToWalletTx(utxoToSpend, spendTimeLock = true)
    println("> Alice deriving witnessScript...")
    val scriptInner = PegIn.descToScriptPubKey(desc)
    println("> Alice derives script signature...")
    val sk = getChildSecretKey(getIndicesByDesc(desc))
    val aliceSig = SignatureBuilder.PegIn.getReclaimSig(getTxSignature(tx, scriptInner, sk.hex))
    println("> Alice adds the witness to the TX...")
    val txWit = WitnessTransaction.toWitnessTx(tx).updateWitness(0, P2WSHWitnessV0(scriptInner, aliceSig))
    println("> Alice submits TX...")
    handleCall(rpcCli.sendRawTransaction(txWit, 0), debug = true).get
    mineBlocks(1)
  }

  def claimTBtc(inputAddress: LockAddress): TransactionId = {
    println("\n============================" + "Alice claims tBTC" + "============================\n")
    val claimAsset = for {
      inputLock <- walletStateApi.getLockByAddress(inputAddress.toBase58()).map(_.get)
      txos      <- genusQueryApi.queryUtxo(inputAddress)
      claimLock <- walletStateApi.getLock("self", "default", 2)
      claimAddr <- txBuilder.lockAddress(claimLock.get)
      claimVk <- walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
        // Derive the verification key at path 1/1/2
        walletApi.deriveChildVerificationKey(VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get), 2)
      }
      _ <- walletStateApi.updateWalletState(
        Encoding.encodeToBase58Check(claimLock.get.getPredicate.toByteArray),
        claimAddr.toBase58(),
        Some("ExtendedEd25519"),
        Some(Encoding.encodeToBase58(claimVk.toByteArray)),
        Indices(1, 1, 2)
      )
      unprovenTx <- txBuilder.buildTransferAllTransaction(
        txos,
        inputLock,
        claimAddr,
        claimAddr,
        0L // TODO: Fee should be able to be a non LVL type
      )
      provenTx <- credentialler.prove(unprovenTx.toOption.get)
      txId     <- bifrostQuery.broadcastTransaction(provenTx)
    } yield (txId, claimAddr)
    val txId = claimAsset.unsafeRunSync()
    Thread.sleep(15000)
    val tbtcBalance = getTbtcBalance(txId._2)
    println(s"Alice owns $tbtcBalance tBTC (claimed)")
    txId._1
  }
}
