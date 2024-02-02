package co.topl.brambl

import akka.actor.ActorSystem
import cats.Id
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.builders.locks.LockTemplate.PredicateTemplate
import co.topl.brambl.builders.locks.PropositionTemplate.{AndTemplate, DigestTemplate, SignatureTemplate, TickTemplate}
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.{BifrostQueryAlgebra, GenusQueryAlgebra, RpcChannelResource}
import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.box.{AssetMintingStatement, Lock}
import co.topl.brambl.models.{Indices, LockAddress, TransactionId}
import co.topl.brambl.playground.BridgeQuery.{PegInRequest, PegInResponse}
import co.topl.brambl.syntax.{bigIntAsInt128, valueToTypeIdentifierSyntaxOps, LvlType}
import co.topl.brambl.utils.Encoding
import co.topl.quivr.api.Proposer
import com.google.protobuf.ByteString
import org.bitcoins.commons.jsonmodels.bitcoind.{BalanceInfo, GetTxOutResultV22}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{NonStandardScriptSignature, P2WSHWitnessV0, RawScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BitcoinAddress, CompactSizeUInt}
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant.{OP_0, ScriptConstant, ScriptToken}
import org.bitcoins.core.script.control.{OP_ELSE, OP_ENDIF, OP_NOTIF}
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_CHECKSIGVERIFY, OP_SHA256}
import org.bitcoins.core.script.locktime.OP_CHECKSEQUENCEVERIFY
import org.bitcoins.core.script.splice.OP_SIZE
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto._
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindAuthCredentials, BitcoindInstanceLocal}
import play.api.libs.json._
import quivr.models.{Digest, Preimage, VerificationKey}
import scodec.bits.ByteVector

import java.io.File
import java.net.URI
import java.nio.file.Paths
import java.security.MessageDigest
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Random, Success, Try}

package object playground {

  def handleCall[T](call: Future[T], debug: Boolean = false): Option[T] = Try {
    Await.result(call, Duration(10, "seconds"))
  } match {
    case Success(value) => Some(value)
    case Failure(exception) =>
      if (debug) println(exception.getMessage)
      None
  }

  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val system: ActorSystem = ActorSystem("System")

  class ExtendedBitcoindRpcClient(instance: BitcoindInstanceLocal) extends BitcoindRpcClient(instance) {

    private def bitcoindCallRaw(
      command:         String,
      parameters:      List[JsValue] = List.empty,
      printError:      Boolean = true,
      uriExtensionOpt: Option[String] = None
    )(implicit reader: Reads[JsValue]): Future[JsValue] = {
      val request =
        buildRequest(instance, command, JsArray(parameters), uriExtensionOpt)
      val responseF = sendRequest(request)

      val payloadF: Future[JsValue] =
        responseF.flatMap(getPayload(_))(ec)

      payloadF
    }

    def listDescriptors(walletName: String, isPrivate: Boolean = false): Future[List[JsValue]] =
      bitcoindCallRaw(
        "listdescriptors",
        List(JsBoolean(isPrivate)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res =>
        (
          res \ "result" \ "descriptors"
        ).result.get.as[JsArray].value.toList
      )(ec)

    def getAddrWitnessProgram(walletName: String, address: String): Future[String] =
      bitcoindCallRaw(
        "getaddressinfo",
        List(JsString(address)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res =>
        (
          res \ "result" \ "witness_program"
        ).result.get.toString()
      )(ec)

    // Takes in desc returns the canonical one (without private keys)
    def getCanonicalDescriptor(walletName: String, desc: String): Future[String] =
      bitcoindCallRaw(
        "getdescriptorinfo",
        List(JsString(desc)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res =>
        (
          (res \ "result" \ "descriptor").result.get.as[JsString].toString().filterNot(_ == '"')
        )
      )(ec)

    def deriveOneAddress(walletName: String, desc: String): Future[String] =
      bitcoindCallRaw(
        "deriveaddresses",
        List(JsString(desc)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res => (res \ "result").result.get.as[JsArray].value.head.as[JsString].toString().filterNot(_ == '"'))(
        ec
      )

    def importDescriptor(walletName: String, desc: String): Future[Boolean] =
      bitcoindCallRaw(
        "importdescriptors",
        List(JsArray(List(JsObject(Map("desc" -> JsString(desc), "timestamp" -> JsString("now")))))),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res =>
        (res \ "result").result.get.as[JsArray].value.head.as[JsObject].value("success").as[JsBoolean].value
      )(
        ec
      )
  }

  object ExtendedBitcoindRpcClient {
    // This (username, password) pair comes from 'rpcuser' and 'rpcpassword' in your bitcoin.conf file
    val AuthCredentials = BitcoindAuthCredentials.PasswordBased("diadem", "NsLbSu6PQc4vYlz")
    // This is the path to your bitcoind executable
    val BitcoindPath = Paths.get("C:", "Program Files", "Bitcoin", "daemon", "bitcoind.exe")

    // Connection to the bitcoind RPC server instance
    val bitcoindInstance =
      BitcoindInstanceLocal(
        network = RegTest,
        uri = new URI(s"http://localhost:${RegTest.port}"),
        rpcUri = new URI(s"http://localhost:${RegTest.rpcPort}"),
        authCredentials = AuthCredentials,
        binary = new File(BitcoindPath.toString)
      )

    def apply(): ExtendedBitcoindRpcClient = new ExtendedBitcoindRpcClient(bitcoindInstance)
  }

  val rpcCli = ExtendedBitcoindRpcClient()

  def mineBlocks(n: Int, wallet: String = "dummy"): Unit = {
    println(s"Mining $n blocks...")
    handleCall(rpcCli.getNewAddress(Some(wallet)).flatMap(rpcCli.generateToAddress(n, _))(ec))
    if (wallet == "dummy") checkBalances()
  }

  def checkBalances(): Unit = {
    def formatBalances(info: BalanceInfo): String =
      s"Trusted: ${info.trusted} | Untrusted_Pending: ${info.untrusted_pending} | Immature: ${info.immature}"

    def printBalance(wallet: String): Unit = {
      println(s"Balance of $wallet: ")
      println(formatBalances(handleCall(rpcCli.getBalances(wallet)).get.mine))
      println(s"# of spendable UTXOs of $wallet: ${handleCall(rpcCli.listUnspent(wallet)).get.length}")
    }

    println("\n===================")
    printBalance("alice")
    println()
    printBalance("bridge")
    println()
    printBalance("watcher")
    println("===================")
  }

  def generateSecret(): Map[String, Array[Byte]] = {
    val secret = Random.nextBytes(24)
    val salt = Random.nextBytes(8) // secret ++ salt has to be 32 bytes
    val hash = MessageDigest.getInstance("SHA-256").digest(secret ++ salt)
    Map(
      "secret" -> secret,
      "salt"   -> salt,
      "hash"   -> hash
    )
  }

  // andor(pk(BridgeVk),sha256(H),and_v(v:pk(AliceVk),older(1000)))
  def generateDescriptor(bridgeVk: String, hash: String, userVk: String): String = {
    val descriptor = s"wsh(andor(pk($bridgeVk),sha256($hash),and_v(v:pk($userVk),older(1000))))"
    // Bridge adds the checksum
    handleCall(rpcCli.getCanonicalDescriptor("bridge", descriptor)).get
  }

  def sizeOf(toPush: String): ScriptConstant = {
    val size = toPush.length / 2
    ScriptConstant("%02x".format(size))
  }

  /**
   * Only works with our specific descriptor
   * We only support 33byte public keys in hex
   * per: BIP-143
   * Each public key passed to a sigop inside version 0 witness program must be a compressed key:
   * the first byte MUST be either 0x02 or 0x03, and the size MUST be 33 bytes.
   *
   * wsh(andor(pk(BridgeVk),sha256(H),and_v(v:pk(AliceVk),older(1000))))
   *
   * <BridgeVk> OP_CHECKSIG OP_NOTIF
   * <AliceVk> OP_CHECKSIGVERIFY <e803> OP_CHECKSEQUENCEVERIFY
   * OP_ELSE
   * OP_SIZE <20> OP_EQUALVERIFY OP_SHA256 <H> OP_EQUAL
   * OP_ENDIF
   */
  def descToScriptPubKey(desc: String): RawScriptPubKey = {
    val bridgeVkStart = desc.indexOf("andor(pk(") + 9
    val bridgeVkEnd = desc.indexOf(")", bridgeVkStart)
    val bridgeVk = desc.substring(bridgeVkStart, bridgeVkEnd)

    val secretStart = desc.indexOf("sha256(") + 7
    val secretEnd = desc.indexOf(")", secretStart)
    val secret = desc.substring(secretStart, secretEnd)
    val secretSize = "%02x".format(32)

    val userVkStart = desc.indexOf("v:pk(") + 5
    val userVkEnd = desc.indexOf(")", userVkStart)
    val userVk = desc.substring(userVkStart, userVkEnd)

    val seqLockTime = BytesUtil.flipEndianness("%02x".format(1000))

    val scriptTokens = Seq(
      sizeOf(bridgeVk), // op codes 1-75 indicates the number of bytes to push
      ScriptConstant(bridgeVk),
      OP_CHECKSIG,
      OP_NOTIF,
      sizeOf(userVk),
      ScriptConstant(userVk),
      OP_CHECKSIGVERIFY,
      sizeOf(seqLockTime),
      ScriptConstant(seqLockTime),
      OP_CHECKSEQUENCEVERIFY,
      OP_ELSE,
      OP_SIZE,
      sizeOf(secretSize),
      ScriptConstant(secretSize),
      OP_EQUALVERIFY,
      OP_SHA256,
      sizeOf(secret),
      ScriptConstant(secret),
      OP_EQUAL,
      OP_ENDIF
    )

    println("script: " + scriptTokens.map(_.hex).mkString(""))
    RawScriptPubKey(scriptTokens)
  }

  /**
   * BIP-143
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
   * We are assuming hashtype is SIGHASH_ALL and sigVersion is SIGVERSION_WITNESS_V0
   *
   * The following was reverse engineered from the bitcoin core implementation
   */
  def serializeForSignature(
    txTo:        Transaction,
    inputAmount: CurrencyUnit, // amount in the output of the previous transaction (what we are spending)
    inputScript: Seq[ScriptToken]
  ): ByteVector = {
    val hashPrevouts: ByteVector = {
      val prevOuts = txTo.inputs.map(_.previousOutput)
      val bytes: ByteVector = BytesUtil.toByteVector(prevOuts)
      CryptoUtil.doubleSHA256(bytes).bytes // result is in little endian
    }

    val hashSequence: ByteVector = {
      val sequences = txTo.inputs.map(_.sequence)
      val littleEndianSeq =
        sequences.foldLeft(ByteVector.empty)(_ ++ _.bytes.reverse)
      CryptoUtil.doubleSHA256(littleEndianSeq).bytes // result is in little endian
    }

    val hashOutputs: ByteVector = {
      val outputs = txTo.outputs
      val bytes = BytesUtil.toByteVector(outputs)
      CryptoUtil.doubleSHA256(bytes).bytes // result is in little endian
    }

    val scriptBytes = BytesUtil.toByteVector(inputScript)

    val i = txTo.inputs.head
    val serializationForSig: ByteVector =
      txTo.version.bytes.reverse ++ hashPrevouts ++ hashSequence ++
      i.previousOutput.bytes ++ CompactSizeUInt.calc(scriptBytes).bytes ++
      scriptBytes ++ inputAmount.bytes ++ i.sequence.bytes.reverse ++
      hashOutputs ++ txTo.lockTime.bytes.reverse ++ Int32(HashType.sigHashAll.num).bytes.reverse
    serializationForSig
  }

  def getTxSignature(unsignedTx: Transaction, script: RawScriptPubKey, privateKey: String): ECDigitalSignature = {
    val inputAmount = handleCall(
      rpcCli.getTxOut(unsignedTx.inputs.head.previousOutput.txIdBE, unsignedTx.inputs.head.previousOutput.vout.toLong)
    ).get.value
    val serializedTxForSignature = serializeForSignature(unsignedTx, inputAmount, script.asm)
    val signableBytes = CryptoUtil.doubleSHA256(serializedTxForSignature)
    val signature = ECPrivateKey.fromHex(privateKey).sign(signableBytes.bytes)
    // append 1 byte hash type onto the end, per BIP-066
    ECDigitalSignature(signature.bytes ++ ByteVector.fromByte(HashType.sigHashAll.byte))
  }

  def sendFromWallet(wallet: String, recipientAddr: String): TransactionOutPoint = {
    val initialFundsUtxo = handleCall(rpcCli.listUnspent(wallet)).get.head
    val unprovenTx = createBaseTx(
      initialFundsUtxo.txid,
      UInt32(initialFundsUtxo.vout),
      BitcoinAddress(recipientAddr),
      initialFundsUtxo.amount.toBigDecimal
    )
    val provenTx = handleCall(rpcCli.signRawTransactionWithWallet(unprovenTx, Some(wallet))).get.hex
    val txId = handleCall(rpcCli.sendRawTransaction(provenTx, 0)).get
    mineBlocks(1)
    TransactionOutPoint(txId, UInt32(0))
  }

  def createBaseTx(
    fromTxId:      DoubleSha256DigestBE,
    fromVOut:      UInt32,
    toAddr:        BitcoinAddress,
    fromAmount:    BigDecimal,
    spendTimeLock: Boolean = false
  ): Transaction = {
    val input = if (spendTimeLock) {
      val sequence: UInt32 = UInt32(1000L & TransactionConstants.sequenceLockTimeMask.toLong)
      TransactionInput(TransactionOutPoint(fromTxId, fromVOut), ScriptSignature.empty, sequence)
    } else TransactionInput.fromTxidAndVout(fromTxId, fromVOut)
    val outputs = Map(toAddr -> Bitcoins(fromAmount - 1)) // 1 BTC as fee
    handleCall(rpcCli.createRawTransaction(Vector(input), outputs)).get
  }

  def createToWalletTx(
    wallet:        String,
    utxoToSpend:   TransactionOutPoint,
    spendTimeLock: Boolean = false
  ): Transaction = {
    val toAddr = handleCall(rpcCli.getNewAddress(Some(wallet))).get
    val inputAmount = handleCall(rpcCli.getTxOut(utxoToSpend.txIdBE, utxoToSpend.vout.toLong)).get.value
    createBaseTx(utxoToSpend.txIdBE, utxoToSpend.vout, toAddr, inputAmount.toBigDecimal, spendTimeLock)
  }

  def verifyTxOutAndGetAmount(txOutPoint: TransactionOutPoint, expectedAddr: String): BigInt = {
    val txOut =
      handleCall(rpcCli.getTxOut(txOutPoint.txIdBE, txOutPoint.vout.toLong)).get.asInstanceOf[GetTxOutResultV22]
    println("    Expected addr: " + expectedAddr)
    println("    Received addr: " + txOut.scriptPubKey.address.get.value)
    println("    amount: " + txOut.value)
    txOut.value.satoshis.toBigInt
  }

  def verifyWitnessScript(wallet: String, script: RawScriptPubKey, expectedAddr: String): Unit = {
    val witnessProgram = handleCall(rpcCli.getAddrWitnessProgram(wallet, expectedAddr)).get
    println("    Expected witness program:" + witnessProgram.filterNot(_ == '"'))
    println("    derived witness program:" + CryptoUtil.sha256(script.asmBytes).hex)
  }

  /**
   * Any witness stack items before the witnessScript are used as the input stack for script evaluation. The input stack
   * is not interpreted as script.
   * For example, there is no need to use a 0x4c (OP_PUSHDATA1) to “push” a big item.
   */
  object ScriptSignatures {

    def getUserReclaimSig(txSig: ECDigitalSignature): ScriptSignature = NonStandardScriptSignature.fromAsm(
      Seq(
        ScriptConstant(txSig.hex), // To satisfy the user's vk
        OP_0 // to fail the bridge's vk
      )
    )

    def getBridgeClaimSig(txSig: ECDigitalSignature, preimageHex: String): ScriptSignature =
      NonStandardScriptSignature.fromAsm(
        Seq(
          ScriptConstant(preimageHex), // To satisfy the hash
          ScriptConstant(txSig.hex) // To satisfy the bridge's vk
        )
      )
  }
  implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

  val channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)
  val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource)
  val bifrostQuery = BifrostQueryAlgebra.make[IO](channelResource)
  val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)

  case class Alice(bridgeRpc: BridgeQuery) extends ToplWallet with BitcoinWallet {
    override val walletName: String = "alice"

    override def initBtcFunds(): Unit = {
      println("Mining 100 blocks to Alice's wallet...")
      mineBlocks(1, walletName)
      mineBlocks(100)
    }

    def initiatePegIn(): String = {
      print("\n============================" + "Alice initiates request" + "============================\n")
      println("> Alice generating 32 byte secret...")
      val secrets = generateSecret()
      println("> Alice saving Preimage and Digest pair to her wallet state...")
      val preimage = Preimage(ByteString.copyFrom(secrets("secret")), ByteString.copyFrom(secrets("salt")))
      val digest = Digest(ByteString.copyFrom(secrets("hash")))
      val digestProposition = Proposer.digestProposer[Id].propose(("Sha256", digest))
      walletStateApi.addPreimage(preimage, digestProposition.getDigest).unsafeRunSync()
      println("> Hardcoding indices to be 5'/5'/5")
      val idx = Indices(5, 5, 5)
      val toplVk = getChildVk(idx)
      val btcKey = getChildSecretKey(idx)
      println("> Alice sending hash and public key to bridge...")
      val resp = bridgeRpc.initiatePegInRequest(
        PegInRequest(Encoding.encodeToHex(secrets("hash")), btcKey.publicKey.hex, toplVk)
      )
      println("> Alice storing descriptor and (toplVk, toplLock, toplIdx) in her wallet state...")
      walletStateApi
        .updateWalletState(
          Encoding.encodeToBase58Check(resp.toplLock.getPredicate.toByteArray),
          resp.toplAddress.toBase58(),
          Some("ExtendedEd25519"),
          Some(Encoding.encodeToBase58(toplVk.toByteArray)),
          idx
        )
        .unsafeRunSync()
      resp.desc
    }

    def initiatePegOut(): Lock.Predicate =
      bridgeRpc.initiatePegOutRequest(???).lock
    def sendTbtcToAddress(lock:     Lock.Predicate): Unit = {}
    def claimBtc(desc:              String): String = ???
    def notifyBridgeBtcClaim(txOut: String): Unit = ???
    def notifyBridgeTbtcTransfer(): String = ???

    def sendBtcToDesc(desc: String): String = {
      print("\n============================" + "Alice sends BTC to Descriptor" + "============================\n")
      println("> Alice deriving address from descriptor...")
      val address = handleCall(rpcCli.deriveOneAddress(walletName, desc)).get
      println("> Alice sends BTC to address...")
      val txOut = sendFromWallet(walletName, address).toHumanReadableString
      txOut
    }

    def reclaimBtc(desc: String): Unit = {
      print("\n============================" + "Alice reclaims BTC" + "============================\n")
      val utxoToSpend = TransactionOutPoint.fromString(getTxOut(desc))
      println("> Alice creating unproven TX...")
      val tx = createToWalletTx(walletName, utxoToSpend, spendTimeLock = true)
      println("> Alice deriving witnessScript...")
      val scriptInner = descToScriptPubKey(desc)
      println("> Alice derives script signature...")
      val sk = getChildSecretKey(getIndicesByDesc(desc))
      val aliceSig = ScriptSignatures.getUserReclaimSig(getTxSignature(tx, scriptInner, sk.hex))
      println("> Alice adds the witness to the TX...")
      val txWit = WitnessTransaction.toWitnessTx(tx).updateWitness(0, P2WSHWitnessV0(scriptInner, aliceSig))
      println("> Alice submits TX...")
      handleCall(rpcCli.sendRawTransaction(txWit, 0), debug = true).get
      mineBlocks(1)
    }

    def notifyBridgeBtcTransfer(txOut: String): LockAddress = {
      println("> Alice informs bridge of Bitcoin TX...")
      bridgeRpc.notifyOfBtcTransfer(txOut)
    }

    def notifyBridgeTbtcClaim(txId: TransactionId, desc: String): Unit = {
      println("> Alice informs bridge of tBTC claim...")
      bridgeRpc.notifyOfTbtcClaim(txId, desc)
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
          0L // TODO: Update txBuilder to take in multiple input locks for different assets (to satisfy fee)
        )
        provenTx <- credentialler.prove(unprovenTx.toOption.get)
        txId     <- bifrostQuery.broadcastTransaction(provenTx)
      } yield txId
      val txId = claimAsset.unsafeRunSync()
      txId
    }
  }

  case class Bridge(secretExtraction: (TransactionId => LockAddress => String)) extends ToplWallet with BitcoinWallet {
    override val walletName: String = "bridge"

    def mintGroupConstructorTokens(): Unit = {
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
      } yield txId
      mintGroup.unsafeRunSync()
    }

    def mintSeriesConstructorTokens(): Unit = {
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
      } yield txId
      mintSeries.unsafeRunSync()
    }

    def mintAssets(toAddr: LockAddress, amount: BigInt): Unit = {
      val mintTBtc = for {
        inputLock    <- walletStateApi.getLock("self", "default", 3)
        inputAddress <- txBuilder.lockAddress(inputLock.get)
        txos         <- genusQueryApi.queryUtxo(inputAddress)
        assetMintingStatement = AssetMintingStatement(
          txos.filter(_.transactionOutput.value.value.isGroup).head.outputAddress,
          txos.filter(_.transactionOutput.value.value.isSeries).head.outputAddress,
          amount
        )
        changeLock    <- walletStateApi.getLock("self", "default", 4)
        changeAddress <- txBuilder.lockAddress(changeLock.get)
        changeVk <- walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
          // Derive the verification key at path 1/1/4 (used in outputLock)
          walletApi.deriveChildVerificationKey(VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get), 4)
        }
        _ <- walletStateApi.updateWalletState(
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
        provenTx <- credentialler.prove(unprovenTx.toOption.get)
        txId     <- bifrostQuery.broadcastTransaction(provenTx)
      } yield txId
      mintTBtc.unsafeRunSync()
    }

    override def initToplFunds(): Unit = {
      super.initToplFunds()
      println("Bridge mints group constructor tokens... waiting 15 seconds")
      mintGroupConstructorTokens()
      Thread.sleep(15000)
      println("Bridge mints series constructor tokens... waiting 15 seconds")
      mintSeriesConstructorTokens()
      Thread.sleep(15000)
    }

    def handleRequest(hash: String, aliceBitcoinVk: String, aliceToplVk: VerificationKey): PegInResponse = {
      print("\n============================" + "Bridge builds Descriptor" + "============================\n")
      println("> Bridge generating keypair...")
      println("> Hardcoding indices to be 5'/5'/5")
      val idx = Indices(5, 5, 5)
      val toplVk = getChildVk(idx)
      val btcKey = getChildSecretKey(idx)
      println("> Bridge generating descriptor...")
      val desc = generateDescriptor(btcKey.hex, hash, aliceBitcoinVk)
      println("> watcher importing descriptor...")
      val importDescSuccessful = handleCall(rpcCli.importDescriptor("watcher", desc)).get
      println("> watcher importing descriptor successful: " + importDescSuccessful)
      println("> Bridge generating descriptor address...")
      val addr = handleCall(rpcCli.deriveOneAddress("bridge", desc)).get
      println("Sending desc: " + desc)
      val hashBytes = Encoding.decodeFromHex(hash).toOption.get
      val toplLock = PredicateTemplate[Id](
        Seq(
          // Alice case
          AndTemplate(
            SignatureTemplate("ExtendedEd25519", 0),
            DigestTemplate("Sha256", Digest(ByteString.copyFrom(hashBytes)))
          ),
          // Bridge case
          AndTemplate(
            SignatureTemplate("ExtendedEd25519", 1),
            TickTemplate(0, Long.MaxValue)
          ) // TODO: Replace with actual current slot
        ),
        1
      ).build(List(aliceToplVk, toplVk)).toOption.get
      val toplAddr = txBuilder.lockAddress(toplLock).unsafeRunSync()
      walletStateApi
        .updateWalletState(
          Encoding.encodeToBase58Check(toplLock.getPredicate.toByteArray),
          toplAddr.toBase58(),
          Some("ExtendedEd25519"),
          Some(Encoding.encodeToBase58(toplVk.toByteArray)),
          idx
        )
        .unsafeRunSync()
      PegInResponse(desc, toplLock, toplAddr)
    }

    def triggerMinting(txOut: String): LockAddress = {
      val utxo = TransactionOutPoint.fromString(txOut)
      val amount = handleCall(rpcCli.getTxOut(utxo.txIdBE, utxo.vout.toLong)).get
        .asInstanceOf[GetTxOutResultV22]
        .value
        .satoshis
        .toBigInt

      println("Bridge mints tBtc... waiting 15 seconds")
      val idx = getIndicesByDesc(getDesc(txOut))
      val lockAddress = (for {
        lock        <- walletStateApi.getLockByIndex(idx)
        lockAddress <- txBuilder.lockAddress(Lock().withPredicate(lock.get))
      } yield lockAddress).unsafeRunSync()
      mintAssets(lockAddress, amount)
      Thread.sleep(15000)
      lockAddress
    }

    def claimBtc(txId: TransactionId, desc: String): Unit = {
      print("\n============================" + "Bridge claims BTC" + "============================\n")
      val utxoToSpend = TransactionOutPoint.fromString(getTxOut(desc))
      println("> Bridge creating unproven TX...")
      val tx = createToWalletTx("bridge", utxoToSpend)
      println("> Bridge deriving witnessScript...")
      val scriptInner = descToScriptPubKey(desc)
      println("> Bridge derives script signature...")
      val idx = getIndicesByDesc(desc)
      val lockAddress = (for {
        lock        <- walletStateApi.getLockByIndex(idx)
        lockAddress <- txBuilder.lockAddress(Lock().withPredicate(lock.get))
      } yield lockAddress).unsafeRunSync()
      val bridgeSig = ScriptSignatures.getBridgeClaimSig(
        getTxSignature(tx, scriptInner, getChildSecretKey(idx).hex),
        secretExtraction(txId)(lockAddress)
      )
      println("> Bridge adds the witness to the TX...")
      val txWit = WitnessTransaction.toWitnessTx(tx).updateWitness(0, P2WSHWitnessV0(scriptInner, bridgeSig))
      println("> Bridge submits TX...")
      handleCall(rpcCli.sendRawTransaction(txWit, 0)).get
      mineBlocks(1)
    }
  }
}
