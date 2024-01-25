package co.topl.brambl

import akka.actor.ActorSystem
import co.topl.brambl.utils.Encoding
import org.bitcoins.commons.jsonmodels.bitcoind.BalanceInfo
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ExtPrivateKey
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindAuthCredentials, BitcoindInstanceLocal}
import play.api.libs.json._

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
      if(debug) println(exception.getMessage)
      None
  }

  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val system: ActorSystem = ActorSystem("System")

  class ExtendedBitcoindRpcClient(instance: BitcoindInstanceLocal) extends BitcoindRpcClient(instance) {
    private def bitcoindCallRaw(
                                 command: String,
                                 parameters: List[JsValue] = List.empty,
                                 printError: Boolean = true,
                                 uriExtensionOpt: Option[String] = None
                               )(implicit reader: Reads[JsValue]): Future[JsValue] = {
      val request =
        buildRequest(instance, command, JsArray(parameters), uriExtensionOpt)
      val responseF = sendRequest(request)

      val payloadF: Future[JsValue] =
        responseF.flatMap(getPayload(_))(ec)

      payloadF
    }

    def listDescriptors(walletName: String, isPrivate: Boolean = false): Future[List[JsValue]] = {
      bitcoindCallRaw(
        "listdescriptors",
        List(JsBoolean(isPrivate)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res => (
        res \ "result" \ "descriptors"
        ).result.get.as[JsArray].value.toList)(ec)

    }

    def getAddrWitnessProgram(walletName: String, address: String): Future[String] = {
      bitcoindCallRaw(
        "getaddressinfo",
        List(JsString(address)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res => (
        res \ "result" \ "witness_program"
        ).result.get.toString())(ec)
    }

    // Takes in desc returns the canonical one (without private keys)
    def getCanonicalDescriptor(walletName: String, desc: String): Future[String] = {
      bitcoindCallRaw(
        "getdescriptorinfo",
        List(JsString(desc)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res => (
        (res \ "result" \ "descriptor").result.get.as[JsString].toString().filterNot(_ == '"')
      ))(ec)
    }
    def deriveOneAddresse(walletName: String, desc: String): Future[String] = {
      bitcoindCallRaw(
        "deriveaddresses",
        List(JsString(desc)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res => {
        (res \ "result").result.get.as[JsArray].value.head.as[JsString].toString().filterNot(_ == '"')
      })(ec)
    }
  }

  object ExtendedBitcoindRpcClient {
    // This (username, password) pair comes from 'rpcuser' and 'rpcpassword' in your bitcoin.conf file
    val AuthCredentials = BitcoindAuthCredentials.PasswordBased("diadem", "NsLbSu6PQc4vYlz")
    // This is the path to your bitcoind executable
    val BitcoindPath = Paths.get("C:", "Program Files", "Bitcoin", "daemon", "bitcoind.exe")
    // Connection to the bitcoind RPC server instance
    val bitcoindInstance = {
      BitcoindInstanceLocal(
        network = RegTest,
        uri = new URI(s"http://localhost:${RegTest.port}"),
        rpcUri = new URI(s"http://localhost:${RegTest.rpcPort}"),
        authCredentials = AuthCredentials,
        binary = new File(BitcoindPath.toString)
      )
    }

    def apply(): ExtendedBitcoindRpcClient = new ExtendedBitcoindRpcClient(bitcoindInstance)
  }

  val rpcCli = ExtendedBitcoindRpcClient()

  def mineBlocks(n: Int, wallet: String = "dummy"): Unit = {
    println(s"Mining $n blocks...")
    handleCall(rpcCli.getNewAddress(Some(wallet)).flatMap(rpcCli.generateToAddress(n, _))(ec))
  }

  def setup(): Unit = {
    println("Setting up wallets...")
    handleCall(rpcCli.createWallet("dummy", descriptors = true))
    handleCall(rpcCli.createWallet("alice", descriptors = true))
    handleCall(rpcCli.createWallet("bridge", descriptors = true))
    mineBlocks(1, "alice")
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
    printBalance("alice")
    println()
    printBalance("bridge")
    println("===================")
  }

  def checkTransaction(txOut: TransactionOutPoint): Unit = {

  }

  case class Secret(secretHex: String, hashHex: String)

  // Secret has to be 32 bytes
  def generateSecret(): Secret = {
    val secret = Random.nextBytes(32)
    val hash = MessageDigest.getInstance("SHA-256").digest(secret)
    Secret(Encoding.encodeToHex(secret), Encoding.encodeToHex(hash))
  }

  case class KeyPair(signingKey: ECPrivateKey, verificationKey: ECPublicKey)
  def getChildKeyPair(wallet: String, i: Int = 4): KeyPair = {
    val rootSecretKeyRaw = (handleCall(rpcCli.listDescriptors(wallet, isPrivate = true)).get.head \ "desc").result.get.toString()
    val rootSecretKeyStr = rootSecretKeyRaw.substring(rootSecretKeyRaw.indexOf("(") + 1, rootSecretKeyRaw.indexOf("/"))
    val keyPath = "m" + rootSecretKeyRaw.substring(rootSecretKeyRaw.indexOf("/"), rootSecretKeyRaw.indexOf(")")).replace("*", i.toString)
    val rootSecretKey = ExtPrivateKey.fromString(rootSecretKeyStr)
    val childSecretKey = rootSecretKey.deriveChildPrivKey(HDPath.fromString(keyPath))
    println(s"Generating child key pair for $wallet at $keyPath...")
    KeyPair(childSecretKey.key, childSecretKey.extPublicKey.key)
  }
}
