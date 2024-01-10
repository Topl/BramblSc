import akka.actor.ActorSystem
import org.bitcoins.commons.jsonmodels.bitcoind.BalanceInfo
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.BitcoinsInt
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.{P2WSHWitnessSPKV0, RawScriptPubKey}
import org.bitcoins.core.protocol.transaction.TransactionInput
import org.bitcoins.core.script.bitwise.OP_EQUAL
import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.script.crypto.OP_HASH160
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindAuthCredentials, BitcoindInstanceLocal}

import java.io.File
import java.net.URI
import java.nio.file.Paths
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

def handleCall[T](call: Future[T]): Option[T] = Try {
  Await.result(call, Duration(10, "seconds"))
} match {
  case Success(value) => Some(value)
  case Failure(exception) =>
    println(exception.getMessage)
    None
}

implicit val ec: ExecutionContext = ExecutionContext.global
implicit val system: ActorSystem = ActorSystem("System")

// This (username, password) pair comes from 'rpcuser' and 'rpcpassword' in your bitcoin.conf file
val authCredentials = BitcoindAuthCredentials.PasswordBased("diadem", "NsLbSu6PQc4vYlz")
// This is the path to your bitcoind executable
val bitcoindPath = Paths.get("C:", "Program Files", "Bitcoin", "daemon", "bitcoind.exe")
// Connection to the bitcoind RPC server instance
val bitcoindInstance = {
  BitcoindInstanceLocal(
    network = RegTest,
    uri = new URI(s"http://localhost:${RegTest.port}"),
    rpcUri = new URI(s"http://localhost:${RegTest.rpcPort}"),
    authCredentials = authCredentials,
    binary = new File(bitcoindPath.toString)
  )
}
val rpcCli = BitcoindRpcClient(bitcoindInstance)

def mineBlocks(n: Int, wallet: String = "dummy"): Unit = {
  println(s"Mining $n blocks...")
  handleCall(rpcCli.getNewAddress(Some(wallet)).flatMap(rpcCli.generateToAddress(n, _)))
}

def setup(): Unit = {
  println("Setting up wallets...")
  handleCall(rpcCli.createWallet("testwallet"))
  handleCall(rpcCli.createWallet("recipient"))
  handleCall(rpcCli.createWallet("dummy"))
  mineBlocks(1, "testwallet")
  mineBlocks(100)
}

def checkBalances(): Unit = {
  def formatBalances(info: BalanceInfo): String = {
    s"Trusted: ${info.trusted} | Untrusted_Pending: ${info.untrusted_pending} | Immature: ${info.immature}"
  }
  def printBalance(wallet: String): Unit = {
    println(s"Balance of $wallet: ")
    println(formatBalances(handleCall(rpcCli.getBalances(wallet)).get.mine))
    println(s"# of spendable UTXOs of $wallet: ${handleCall(rpcCli.listUnspent(wallet)).get.length}")
  }
  println("===================")
  printBalance("testwallet")
  println()
  printBalance("recipient")
  println("===================")
}


/**
 * Create a TX that spends the 50 BTC UTXO into 3 outputs with 15, 15, and 15 BTC respectively, locking each output with a different lock.
 *
 * Per the bitcoin.conf file, betch32 is the default address format. This means that the 50 BTC are locked up in a P2WPKH address
 * under the "testwallet".
 *
 * The first output will be locked to a new P2WPKH address in the "testwallet".
 * The second output will be locked to a P2WSH address that will imported to the "testwallet".
 * the last output will be locked to a an address that was received externally from a recipient (from the "recipient" wallet).
 * This means that the second output will be also locked to a P2WSH address.
 *
 * 1BTC will be used as a fee
 */
def firstTx(preimage: String): Unit = {
  println("Creating first TX...")
  val utxo = handleCall(rpcCli.listUnspent("testwallet")).get.head
  val inputs = Vector(TransactionInput.fromTxidAndVout(utxo.txid, UInt32(utxo.vout)))  // The input is the 50 BTC UTXO

  // The first output will be locked to a new P2WPKH address (Betch32) in the "testwallet".
  val testWalletAddr = handleCall(rpcCli.getNewAddress(Some("testwallet"))).get

  // The second output will be locked to a new P2WSH address (Betch32).
  // Does not belong to any wallet since anyone with the preimage can spend it
  val digest = CryptoUtil.sha256Hash160(preimage).bytes
  val script = RawScriptPubKey(Seq(OP_HASH160, ScriptConstant(digest), OP_EQUAL)) // OP_HASH160 <digest> OP_EQUAL
  val digestAddr = BitcoinAddress.fromScriptPubKey(P2WSHWitnessSPKV0(script), RegTest)  // P2WSHWitnessSPKV0 will build us a P2WSH scriptPubKey

  // The last output will be locked to a new P2WPKH address (Betch32) in the "recipient".
  val recipientAddr = BitcoinAddress(handleCall(rpcCli.getNewAddress(Some("recipient"))).get.value)

  val outputs = Map(testWalletAddr -> 15.bitcoins, digestAddr -> 19.bitcoins, recipientAddr -> 15.bitcoins)
  val unprovenTx = handleCall(rpcCli.createRawTransaction(inputs, outputs)).get
  val provenTx = handleCall(rpcCli.signRawTransactionWithWallet(unprovenTx, Some("testwallet"))).get.hex

//  println(unprovenTx)
//  println(provenTx)

  handleCall(rpcCli.sendRawTransaction(provenTx, 0))
}
//
///**
// * Create a TX that spends the 19 BTC locked in the digest script and sends it to the "recipient" wallet.
// */
//def secondTx(preimage: String): Unit = {
//
//}
//
//setup()
//checkBalances()
//
//
//val preimage = "very secret message"
//firstTx(preimage)
//checkBalances()
//
//
//mineBlocks(1)
//checkBalances()
println(handleCall(rpcCli.listUnspent))