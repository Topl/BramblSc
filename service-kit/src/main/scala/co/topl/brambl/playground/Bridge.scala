package co.topl.brambl.playground

import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.{AssetMintingStatement, Lock}
import co.topl.brambl.models.{Indices, LockAddress, TransactionId, TransactionOutputAddress}
import co.topl.brambl.playground.ScriptBuilder.PegIn
import co.topl.brambl.playground.SecretExtraction.{extractFromBitcoinTx, extractFromToplTx}
import co.topl.brambl.syntax.{AssetType, LvlType, bigIntAsInt128, groupPolicyAsGroupPolicySyntaxOps, int128AsBigInt, seriesPolicyAsSeriesPolicySyntaxOps, valueToQuantitySyntaxOps, valueToTypeIdentifierSyntaxOps}
import co.topl.brambl.utils.Encoding
import org.bitcoins.commons.jsonmodels.bitcoind.GetTxOutResultV22
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.{TransactionOutPoint, WitnessTransaction}
import org.bitcoins.crypto.DoubleSha256DigestBE
import quivr.models.VerificationKey

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
        inputLock    <- walletStateApi.getLock("self", "default", 2)
        inputAddress <- txBuilder.lockAddress(inputLock.get)
        txos         <- genusQueryApi.queryUtxo(inputAddress)
        seriesPolicy = SeriesPolicy(
          "tBTC Series",
          registrationUtxo = txos.filter(_.transactionOutput.value.value.typeIdentifier == LvlType).head.outputAddress
        )
        outputLock    <- walletStateApi.getLock("self", "default", 3)
        outputAddress <- txBuilder.lockAddress(outputLock.get)
        outputVk <- walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
          // Derive the verification key at path 1/1/3 (used in outputLock)
          walletApi.deriveChildVerificationKey(VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get), 3)
        }
        _ <- walletStateApi.updateWalletState(
          Encoding.encodeToBase58Check(outputLock.get.getPredicate.toByteArray),
          outputAddress.toBase58(),
          Some("ExtendedEd25519"),
          Some(Encoding.encodeToBase58(outputVk.toByteArray)),
          Indices(1, 1, 3)
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
      println("Bridge mints group constructor tokens... waiting 15 seconds")
      val gp = mintGroupConstructorTokens()
      Thread.sleep(15000)
      println("Bridge mints series constructor tokens... waiting 15 seconds")
      val sp = mintSeriesConstructorTokens()
      Thread.sleep(15000)
      AssetType(gp.computeId.value, sp.computeId.value).some
    }
  }
  val btcWallet = new BitcoinWallet(walletName)

  val monitoringService: MonitoringService = MonitoringService()

  def init(): Unit = {
    val tbtc = toplWallet.initToplFunds().get
    monitoringService.start(tbtc, claimBtc)
  }

  init()

  def mintAssets(toAddr: LockAddress, amount: BigInt): Unit = {
    val mintTBtc = for {
      inputLock    <- toplWallet.walletStateApi.getLock("self", "default", 3)
      inputAddress <- txBuilder.lockAddress(inputLock.get)
      txos         <- genusQueryApi.queryUtxo(inputAddress)
      assetMintingStatement = AssetMintingStatement(
        txos.filter(_.transactionOutput.value.value.isGroup).head.outputAddress,
        txos.filter(_.transactionOutput.value.value.isSeries).head.outputAddress,
        amount
      )
      changeLock    <- toplWallet.walletStateApi.getLock("self", "default", 4)
      changeAddress <- txBuilder.lockAddress(changeLock.get)
      changeVk <- toplWallet.walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
        // Derive the verification key at path 1/1/4 (used in outputLock)
        toplWallet.walletApi.deriveChildVerificationKey(
          VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get),
          4
        )
      }
      _ <- toplWallet.walletStateApi.updateWalletState(
        Encoding.encodeToBase58Check(changeLock.get.getPredicate.toByteArray),
        changeAddress.toBase58(),
        Some("ExtendedEd25519"),
        Some(Encoding.encodeToBase58(changeVk.toByteArray)),
        Indices(1, 1, 4)
      )
      unprovenTx <- txBuilder.buildAssetMintingTransaction(
        assetMintingStatement,
        txos,
        Map(inputAddress -> inputLock.get.getPredicate),
        1L,
        toAddr,
        changeAddress
      )
      provenTx <- toplWallet.credentialler.prove(unprovenTx.toOption.get)
      txId     <- bifrostQuery.broadcastTransaction(provenTx)
    } yield txId
    mintTBtc.unsafeRunSync()
  }

  def triggerMinting(txOut: String, desc: String): LockAddress = {
    val utxo = TransactionOutPoint.fromString(txOut)
    val amount = handleCall(rpcCli.getTxOut(utxo.txIdBE, utxo.vout.toLong)).get
      .asInstanceOf[GetTxOutResultV22]
      .value
      .satoshis
      .toBigInt

    println("Bridge mints tBtc... waiting 15 seconds")
    btcWallet.addDescTxOutEntry(desc, txOut)
    val idx = btcWallet.getIndicesByDesc(desc)
    val lockAddress = (for {
      lock        <- toplWallet.walletStateApi.getLockByIndex(idx)
      lockAddress <- txBuilder.lockAddress(Lock().withPredicate(lock.get))
    } yield lockAddress).unsafeRunSync()
    mintAssets(lockAddress, amount)
    Thread.sleep(15000)
    val tbtcBalance = toplWallet.getTbtcBalance(lockAddress)
    println(s"Bridge minted $tbtcBalance tBTC (unclaimed)")
    lockAddress
  }
  def claimBtc(txId: TransactionId, lockAddr: LockAddress): Unit = {
    print("\n============================" + "Bridge claims BTC" + "============================\n")
    val desc = btcWallet.getDescByAddress(lockAddr)
    val utxoToSpend = TransactionOutPoint.fromString(btcWallet.getTxOut(desc))
    println("> Bridge creating unproven TX...")
    val tx = btcWallet.createToWalletTx(utxoToSpend)
    println("> Bridge deriving witnessScript...")
    val scriptInner = PegIn.descToScriptPubKey(desc)
    println("> Bridge derives script signature...")
    val idx = btcWallet.getIndicesByDesc(desc)
    val lockAddress = (for {
      lock        <- toplWallet.walletStateApi.getLockByIndex(idx)
      lockAddress <- txBuilder.lockAddress(Lock().withPredicate(lock.get))
    } yield lockAddress).unsafeRunSync()
    val bridgeSig = SignatureBuilder.PegIn.getClaimSig(
      getTxSignature(tx, scriptInner, btcWallet.getChildSecretKey(idx).hex),
      extractFromToplTx(txId, lockAddress)
    )
    println("> Bridge adds the witness to the TX...")
    val txWit = WitnessTransaction.toWitnessTx(tx).updateWitness(0, P2WSHWitnessV0(scriptInner, bridgeSig))
    println("> Bridge submits TX...")
    handleCall(rpcCli.sendRawTransaction(txWit, 0)).get
    mineBlocks(1)
  }

  def claimTbtc(txId: DoubleSha256DigestBE, desc: String): Unit = {
    println("\n============================" + "Bridge claims tBTC" + "============================\n")
    val tx = handleCall(rpcCli.getTransaction(txId, walletNameOpt = Some(btcWallet.watcherName))).get.hex
    val preimage = extractFromBitcoinTx(tx, desc)
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
      claimLock    <- toplWallet.walletStateApi.getLock("self", "default", 5)
      claimAddr    <- txBuilder.lockAddress(claimLock.get)
      claimVk <- toplWallet.walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
        // Derive the verification key at path 1/1/5
        toplWallet.walletApi.deriveChildVerificationKey(
          VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get),
          5
        )
      }
      _ <- toplWallet.walletStateApi.updateWalletState(
        Encoding.encodeToBase58Check(claimLock.get.getPredicate.toByteArray),
        claimAddr.toBase58(),
        Some("ExtendedEd25519"),
        Some(Encoding.encodeToBase58(claimVk.toByteArray)),
        Indices(1, 1, 5)
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
    } yield (txId, claimAddr, provenTx, inputAddress)
    val newTxId = claimAsset.unsafeRunSync()
    Thread.sleep(15000)
    val tbtcBalance = toplWallet.getTbtcBalance(newTxId._2)
    println(s"Bridge owns $tbtcBalance tBTC (claimed)")
  }

  def triggerBtcTransfer(utxoId: TransactionOutputAddress): String = {
    val utxo = (for {
      tx <- bifrostQuery.fetchTransaction(utxoId.id)
    } yield tx.get.outputs(utxoId.index)).unsafeRunSync()
    val desc = (for {
      idx <- toplWallet.walletStateApi.getIndicesByAddress(utxo.address.toBase58())
    } yield btcWallet.getDescByIndices(idx.get)).unsafeRunSync()
    btcWallet.sendBtcToDesc(desc, (utxo.value.value.quantity: BigInt).some)
  }
}
