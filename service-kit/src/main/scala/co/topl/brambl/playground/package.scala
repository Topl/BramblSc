package co.topl.brambl

import akka.actor.ActorSystem
import cats.arrow.FunctionK
import cats.effect.IO
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.{BifrostQueryAlgebra, GenusQueryAlgebra, RpcChannelResource}
import org.bitcoins.commons.jsonmodels.bitcoind.BalanceInfo
import org.bitcoins.core.currency.{Bitcoins, BitcoinsInt, CurrencyUnit}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{RawScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BitcoinAddress, CompactSizeUInt}
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto._
import play.api.libs.json._
import scodec.bits.ByteVector

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

  val rpcCli = ExtendedBitcoindRpcClient()
  handleCall(rpcCli.createWallet("dummy", descriptors = true))

  def mineBlocks(n: Int, wallet: String = "dummy"): Unit = {
    println(s"Mining $n blocks to $wallet...")
    handleCall(rpcCli.getNewAddress(Some(wallet)).flatMap(rpcCli.generateToAddress(n, _))(ec))
  }

  def formatBalances(info: BalanceInfo): String =
    s"Trusted: ${info.trusted} | Untrusted_Pending: ${info.untrusted_pending} | Immature: ${info.immature}"

  val SecretSize = 24

  def generateSecret(): Map[String, Array[Byte]] = {
    val secret = Random.nextBytes(SecretSize)
    val salt = Random.nextBytes(32 - SecretSize) // secret ++ salt has to be 32 bytes
    val hash = MessageDigest.getInstance("SHA-256").digest(secret ++ salt)
    Map(
      "secret" -> secret,
      "salt"   -> salt,
      "hash"   -> hash
    )
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

  def createBaseTx(
    fromTxId:      DoubleSha256DigestBE,
    fromVOut:      UInt32,
    toAddr:        BitcoinAddress,
    fromAmount:    Bitcoins,
    spendTimeLock: Boolean = false
  ): Transaction = {
    val input = if (spendTimeLock) {
      val sequence: UInt32 = UInt32(1000L & TransactionConstants.sequenceLockTimeMask.toLong)
      TransactionInput(TransactionOutPoint(fromTxId, fromVOut), ScriptSignature.empty, sequence)
    } else TransactionInput.fromTxidAndVout(fromTxId, fromVOut)
    println(s"input: $input")
    val outputs = Map(toAddr -> Bitcoins(fromAmount.toBigDecimal - 1)) // 1 BTC as fee
    println(s"Creating tx with input: $input and outputs: $outputs")
    handleCall(rpcCli.createRawTransaction(Vector(input), outputs)).get
  }

  def fundFromDummy(recipientAddr: BitcoinAddress): Unit = {
    val utxo = handleCall(rpcCli.listUnspent("dummy")).get.find(_.amount > 25.bitcoins).get
    // The input has moret than 25BTC
    val inputs = Vector(TransactionInput.fromTxidAndVout(utxo.txid, UInt32(utxo.vout)))

    val changeAddr = handleCall(rpcCli.getNewAddress(Some("dummy"))).get
    val outputs = Map(recipientAddr -> 25.bitcoins, changeAddr -> 24.bitcoins) // 1 btc fee

    val unprovenTx = handleCall(rpcCli.createRawTransaction(inputs, outputs)).get
    val provenTx = handleCall(rpcCli.signRawTransactionWithWallet(unprovenTx, Some("dummy"))).get.hex
    println("Sending: ")
    println(provenTx)

    handleCall(rpcCli.sendRawTransaction(provenTx, 0)).get
    mineBlocks(1)
  }

  implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

  val channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)
  val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource)
  val bifrostQuery = BifrostQueryAlgebra.make[IO](channelResource)
  val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)
}
