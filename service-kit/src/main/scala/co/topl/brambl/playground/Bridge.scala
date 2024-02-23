package co.topl.brambl.playground

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.{catsSyntaxOptionId, toTraverseOps}
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.{AssetMintingStatement, Attestation, Lock}
import co.topl.brambl.models.{Indices, LockAddress, TransactionOutputAddress}
import co.topl.brambl.playground.ScriptBuilder.PegIn
import co.topl.brambl.playground.SecretExtraction.{extractFromBitcoinTx, extractFromToplTx}
import co.topl.brambl.syntax.{AssetType, LvlType, bigIntAsInt128, groupPolicyAsGroupPolicySyntaxOps, int128AsBigInt, seriesPolicyAsSeriesPolicySyntaxOps, valueToQuantitySyntaxOps, valueToTypeIdentifierSyntaxOps}
import co.topl.brambl.utils.Encoding
import org.bitcoins.commons.jsonmodels.bitcoind.GetTxOutResultV22
import org.bitcoins.core.protocol.script.{P2WSHWitnessV0, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{TransactionOutPoint, WitnessTransaction}
import quivr.models.VerificationKey

import scala.concurrent.duration.DurationInt

case class Bridge() {
  val walletName: String = "bridge"

  val toplWallet = new ToplWallet(walletName) {

    def mintGroupConstructorTokens(): GroupPolicy = {
      val mintGroup = for {
        inputLock    <- walletStateApi.getLock("self", "default", 1)
        inputAddress <- txBuilder.lockAddress(inputLock.get)
        txos         <- genusQueryApi.queryUtxo(inputAddress)
        groupPolicy = GroupPolicy("tBTC Group", txos.head.outputAddress)
        outputLock    <- walletStateApi.getLock("self", "default", 2)
        outputAddress <- txBuilder.lockAddress(outputLock.get)
        outputVk <- walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
          // Derive the verification key at path 1/1/2 (used in outputLock)
          walletApi.deriveChildVerificationKey(VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get), 2)
        }
        _ <- walletStateApi.updateWalletState(
          Encoding.encodeToBase58Check(outputLock.get.getPredicate.toByteArray),
          outputAddress.toBase58(),
          Some("ExtendedEd25519"),
          Some(Encoding.encodeToBase58(outputVk.toByteArray)),
          Indices(1, 1, 2)
        )
        unprovenTx <- txBuilder.buildGroupMintingTransaction(
          txos,
          inputLock.get.getPredicate,
          groupPolicy,
          1L,
          outputAddress,
          outputAddress,
          1L
        )
        provenTx <- credentialler.prove(unprovenTx.toOption.get)
        txId     <- bifrostQuery.broadcastTransaction(provenTx)
      } yield (txId, groupPolicy)
      val res = mintGroup.unsafeRunSync()
      res._2
    }

    def mintSeriesConstructorTokens(): SeriesPolicy = {
      val mintSeries = for {
        idx <- walletStateApi.getNextIndicesForFunds("self", "default").map(_.get)
        inputLock    <- walletStateApi.getLock("self", "default", idx.z - 1)
        inputAddress <- txBuilder.lockAddress(inputLock.get)
        txos         <- genusQueryApi.queryUtxo(inputAddress)
        seriesPolicy = SeriesPolicy(
          "tBTC Series",
          registrationUtxo = txos.filter(_.transactionOutput.value.value.typeIdentifier == LvlType).head.outputAddress
        )
        outputLock    <- walletStateApi.getLock("self", "default", idx.z)
        outputAddress <- txBuilder.lockAddress(outputLock.get)
        outputVk <- walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
          walletApi.deriveChildVerificationKey(VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get), idx.z)
        }
        _ <- walletStateApi.updateWalletState(
          Encoding.encodeToBase58Check(outputLock.get.getPredicate.toByteArray),
          outputAddress.toBase58(),
          Some("ExtendedEd25519"),
          Some(Encoding.encodeToBase58(outputVk.toByteArray)),
          idx
        )
        unprovenTx <- txBuilder.buildSeriesMintingTransaction(
          txos,
          inputLock.get.getPredicate,
          seriesPolicy,
          1L,
          outputAddress,
          outputAddress,
          1L
        )
        provenTx <- credentialler.prove(unprovenTx.toOption.get)
        txId     <- bifrostQuery.broadcastTransaction(provenTx)
      } yield (txId, seriesPolicy)
      val res = mintSeries.unsafeRunSync()
      res._2
    }

    override def initToplFunds(): Option[AssetType] = {
      super.initToplFunds()
      println("Bridge mints group constructor tokens... waiting 20 seconds")
      val gp = mintGroupConstructorTokens()
      Thread.sleep(20000)
      println("Bridge mints series constructor tokens... waiting 20 seconds")
      val sp = mintSeriesConstructorTokens()
      Thread.sleep(20000)
      AssetType(gp.computeId.value, sp.computeId.value).some
    }
  }
  val btcWallet = new BitcoinWallet(walletName)

  def init(): Unit =
    toplWallet.initToplFunds()

  init()

  def mintAssets(toAddr: LockAddress, amount: BigInt): Unit = {
    println(s"Bridge mints $amount tBTC to ${toAddr.toBase58()}")
    val mintTBtc = for {
      balance <- toplWallet.walletStateApi.getCurrentAddresses()
      allTxos <- balance.map(b => (b -> genusQueryApi.queryUtxo(b._2)).sequence).sequence
      txosToUse = allTxos.filter(txos =>
        txos._2.exists(_.transactionOutput.value.value.isGroup) && txos._2.exists(_.transactionOutput.value.value.isSeries)
      ).maxBy(v => (v._1._1.x, v._1._1.y, v._1._1.z))
      inputAddress    = txosToUse._1._2
      inputLock    <- toplWallet.walletStateApi.getLockByAddress(inputAddress.toBase58()).map(_.get)
      txos         = txosToUse._2
      assetMintingStatement = AssetMintingStatement(
        txos.filter(_.transactionOutput.value.value.isGroup).head.outputAddress,
        txos.filter(_.transactionOutput.value.value.isSeries).head.outputAddress,
        amount
      )
      // No longer hardcoding indices
      changeIdx     <- toplWallet.walletStateApi.getNextIndicesForFunds("self", "default").map(_.get)
      changeLock    <- {
        println(s"input idx: ${txosToUse._1._1}")
        println(s"change idx: $changeIdx")
        toplWallet.walletStateApi.getLock("self", "default", changeIdx.z)
      }
      changeAddress <- txBuilder.lockAddress(changeLock.get)
      changeVk <- toplWallet.walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
        toplWallet.walletApi.deriveChildVerificationKey(
          VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get),
          changeIdx.z
        )
      }
      _ <- toplWallet.walletStateApi.updateWalletState(
        Encoding.encodeToBase58Check(changeLock.get.getPredicate.toByteArray),
        changeAddress.toBase58(),
        Some("ExtendedEd25519"),
        Some(Encoding.encodeToBase58(changeVk.toByteArray)),
        changeIdx
      )
      unprovenTx <- txBuilder.buildAssetMintingTransaction(
        assetMintingStatement,
        txos,
        Map(inputAddress -> inputLock),
        1L,
        toAddr,
        changeAddress
      )
      provenTx <- toplWallet.credentialler.prove(unprovenTx.toOption.get)
      txId     <- bifrostQuery.broadcastTransaction(provenTx)
    } yield txId
    mintTBtc.unsafeRunSync()
    (genusQueryApi.queryUtxo(toAddr).iterateWhile(_.isEmpty) *> IO.println(s"Bridge minted $amount tBTC to ${toAddr.toBase58()}")).unsafeRunSync()
    displayBalance()
  }

  def triggerMinting(txOut: String, desc: String): LockAddress = {
    println(s"Bridge mints tBTC b/c BTC was sent to $desc")
    val utxo = TransactionOutPoint.fromString(txOut)
    val amount = handleCall(rpcCli.getTxOut(utxo.txIdBE, utxo.vout.toLong)).get
      .asInstanceOf[GetTxOutResultV22]
      .value
      .satoshis
      .toBigInt

    btcWallet.addDescTxOutEntry(desc, txOut)
    val idx = btcWallet.getIndicesByDesc(desc)
    val lockAddress = (for {
      lock        <- toplWallet.walletStateApi.getLockByIndex(idx)
      lockAddress <- txBuilder.lockAddress(Lock().withPredicate(lock.get))
    } yield lockAddress).unsafeRunSync()
    mintAssets(lockAddress, amount)
    println(s"Bridge minted ${amount} tBTC (unclaimed)")
    IO.unit.andWait(20.seconds).unsafeRunSync()
    lockAddress
  }

  // TODO: Handle the case where the bridge reclaimed the TBTC.. if that's the case, then the BTC will not be available to spend anymore
  // case 1: User claimed TBTC
  // case 2: Bridge reclaimed BTC
  def claimBtc(att: Attestation, lockAddr: LockAddress): Unit = {
    print("\n============================" + "Bridge claims BTC" + "============================\n")
    val desc = btcWallet.getDescByAddress(lockAddr)
    val utxoToSpend = TransactionOutPoint.fromString(btcWallet.getTxOut(desc))
    val isSpendable = handleCall(rpcCli.listUnspent(walletName = "bridge-watcher")
      .map(_.exists(utxo => utxoToSpend.txIdBE == utxo.txid && utxoToSpend.vout.toLong == utxo.vout))).get
    if(isSpendable) { // BTC is not claimed yet, bridge should claim
      println("> Bridge creating unproven TX...")
      val tx = btcWallet.createToWalletTx(utxoToSpend)
      println("> Bridge deriving witnessScript...")
      val scriptInner = PegIn.descToScriptPubKey(desc)
      println("> Bridge derives script signature...")
      val idx = btcWallet.getIndicesByDesc(desc)
      val bridgeSig = SignatureBuilder.PegIn.getClaimSig(
        getTxSignature(tx, scriptInner, btcWallet.getChildSecretKey(idx).hex),
        extractFromToplTx(att)
      )
      println("> Bridge adds the witness to the TX...")
      val txWit = WitnessTransaction.toWitnessTx(tx).updateWitness(0, P2WSHWitnessV0(scriptInner, bridgeSig))
      println("> Bridge submits TX...")
      handleCall(rpcCli.sendRawTransaction(txWit, 0)).get
      mineBlocks(1)
      displayBalance()
    } else { // BTC is already claimed (from user). Nothing to do
      println("BTC is already claimed... nothing to do")
    }
  }

  // TODO: Handle the case where the bridge reclaimed the BTC.. if that's the case, then the TBTC will not be available to spend anymore
  def claimTbtc(proof: ScriptWitness, desc: String): Unit = {
    println("\n============================" + "Bridge claims tBTC" + "============================\n")
    val preimage = extractFromBitcoinTx(proof)
    val idx = btcWallet.getIndicesByDesc(desc) // should be 5/5/6
    val digestProp = toplWallet.walletStateApi
      .getLockByIndex(idx)
      .map(_.get.challenges.head.getRevealed.getAnd.right.getDigest)
      .unsafeRunSync()
    println(preimage, digestProp)
    toplWallet.walletStateApi.addPreimage(preimage, digestProp).unsafeRunSync()
    val claimAsset = for {
      inputLock    <- toplWallet.walletStateApi.getLockByIndex(idx).map(_.get)
      inputAddress <- txBuilder.lockAddress(Lock().withPredicate(inputLock))
      txos         <- genusQueryApi.queryUtxo(inputAddress)
      trivialAddr <- toplWallet.walletStateApi.getAddress("nofellowship", "genesis", None)
        .map(addr => AddressCodecs.decodeAddress(addr.get).toOption.get)
      unprovenTx <- txBuilder.buildTransferAllTransaction(
        txos,
        inputLock,
        trivialAddr, // trivial, will remove
        trivialAddr,
        0L // TODO: Fee should be able to be a non LVL type
      )
      provenTx <- toplWallet.credentialler.prove(unprovenTx.toOption.get)
      txId     <- bifrostQuery.broadcastTransaction(provenTx)
    } yield (txId, provenTx, inputAddress)
    claimAsset.unsafeRunSync()
    Thread.sleep(15000)
    println(s"Bridge burned tBTC")
    displayBalance()
  }

  def triggerBtcTransfer(utxoId: TransactionOutputAddress): String = {
    val utxo = (for {
      tx <- bifrostQuery.fetchTransaction(utxoId.id)
    } yield tx.get.outputs(utxoId.index)).unsafeRunSync()
    val desc = (for {
      idx <- toplWallet.walletStateApi.getIndicesByAddress(utxo.address.toBase58())
    } yield btcWallet.getDescByIndices(idx.get)).unsafeRunSync()
    val txId = btcWallet.sendBtcToDesc(desc, (utxo.value.value.quantity: BigInt).some)
    displayBalance()
    txId
  }

  def reclaimTbtc(desc: String): Unit = {
    // At this point in time, we don't know what triggered this.. The bridge could have claimed BTC, or the user could have reclaimed BTC
    // 1: If the user reclaimed BTC, then the TBTC is still available to be claimed
    // 2: If the bridge claimed the BTC, then the TBTC is no longer available to be claimed (the bridge needs to extract the secret from the Topl TX)
    // Therefore, if the topl LockAddress is still unspent, then we know that the user has reclaimed their BTC, and the bridge can reclaim the TBTC
    val idx = btcWallet.getIndicesByDesc(desc)
    println(s"reclaim tbtc, idx: $idx")
    (for {
      lock        <- toplWallet.walletStateApi.getLockByIndex(idx).map(_.get)
      lockAddress <- txBuilder.lockAddress(Lock().withPredicate(lock))
      txos        <- genusQueryApi.queryUtxo(lockAddress) // default is unspent
      // In production, we could check for the specific TBTC asset type
      tbtc = txos.find(_.transactionOutput.value.value.isAsset)
    } yield tbtc match {
      case None => () // if the user already claimed the TBTC, do nothing; case 2
      case Some(_) =>  // if the user has not claimed the TBTC, the bridge reclaims it; case 1
        (for {
          trivialAddr <- toplWallet.walletStateApi.getAddress("nofellowship", "genesis", None)
            .map(addr => AddressCodecs.decodeAddress(addr.get).toOption.get)
          unprovenTx <- txBuilder.buildTransferAllTransaction(
            txos,
            lock,
            trivialAddr,
            trivialAddr,
            0L // TODO: Fee should be able to be a non LVL type
          )
          provenTx <- toplWallet.credentialler.prove(unprovenTx.toOption.get)
          txId     <- bifrostQuery.broadcastTransaction(provenTx)
        } yield txId).unsafeRunSync()
        Thread.sleep(15000)
        println(s"Bridge burned tBTC")
    }).unsafeRunSync()
    displayBalance()
  }

  def displayBalance(): Unit = {
    val balance = Seq(
      toplWallet.getBalance(),
      btcWallet.getBalance()
    ) mkString (
      s"==================$walletName Topl Balance===================",
      "===================Bitcoin Balance=====================",
      "====================================="
    )
    println(balance)
  }
}
