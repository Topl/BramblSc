package co.topl.brambl

import akka.actor.ActorSystem
import org.bitcoins.commons.jsonmodels.bitcoind.BalanceInfo
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.BitcoinsInt
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.TransactionInput
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindAuthCredentials, BitcoindInstanceLocal}
import play.api.libs.json._

import java.io.File
import java.net.URI
import java.nio.file.Paths
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
package object playground {

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

    def getNewPublicKey(walletName: String): Future[String] = {
      this.getNewAddress(Some(walletName)).flatMap(addr => {
        this.getAddressInfo(addr, Some(walletName)).map(_.pubkey.get.hex)(ec)
    })(ec)
    }
    def getAddrWitnessProgran(walletName: String, address: String): Future[String] = {
      bitcoindCallRaw(
        "getaddressinfo",
        List(JsString(address)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res => (
        res \ "result" \ "witness_program"
        ).result.get.toString())(ec)
    }
    def getAddrPubKey(walletName: String, address: String): Future[String] = {
      bitcoindCallRaw(
        "getaddressinfo",
        List(JsString(address)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res => (
        res \ "result" \ "scriptPubKey"
        ).result.get.toString())(ec)
    }

    def getDescriptor(walletName: String, isPrivate: Boolean = true): Future[String] = {
      this.listDescriptors(walletName, isPrivate = isPrivate).map(descriptors => {
        descriptors
          .map(d => (d \ "desc").get.as[JsString].toString().filterNot(_ == '"'))
          .find(_.startsWith("pkh(")).get
      })(ec)
    }

    // desc should be a descriptor with checksum
    def importDescriptor(walletName: String, desc: String): Future[JsValue] = {
      bitcoindCallRaw(
        "importdescriptors",
        List(Json.toJson(List(Map(
          "desc" -> desc,
          "timestamp" -> "now",
          "label" -> "pegIn",
        )))),
        uriExtensionOpt = Some(walletExtension(walletName))
      )
//        .map(res => (
//        res \ "result" \ "descriptor"
//        ).result.get.as[JsString].toString().filterNot(_ == '"'))(ec)
    }

    // Takes in desc (with or without checksum), returns the canonical one (without private keys) and orig checksum
    // useful for importing descriptors with private keys
    def applyDescriptorChecksum(walletName: String, desc: String): Future[Map[String, String]] = {
      bitcoindCallRaw(
        "getdescriptorinfo",
        List(JsString(desc)),
        uriExtensionOpt = Some(walletExtension(walletName))
      ).map(res => {
        val full = (res \ "result").result.get.as[JsObject].value
        Map(
          "descriptor" -> full("descriptor").as[JsString].toString().filterNot(_ == '"'),
          "checksum" -> full("checksum").as[JsString].toString().filterNot(_ == '"'),
        )
      })(ec)
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

  def extractKey(desc: String, starChar: Char): String = {
    val start = desc.indexOf(starChar) + 1
    val end = desc.indexOf(')')
    desc.substring(start, end)
  }

  def extractXPubKey(desc: String): String = extractKey(desc, ']')
  def extractXPrivKey(desc: String): String = extractKey(desc, '(')

  def mineBlocks(n: Int, wallet: String = "dummy"): Unit = {
    println(s"Mining $n blocks...")
    handleCall(rpcCli.getNewAddress(Some(wallet)).flatMap(rpcCli.generateToAddress(n, _))(ec))
  }

  def setup(): Unit = {
    println("Setting up wallets...")
//    handleCall(rpcCli.createWallet("testwallet", descriptors = true))
//    handleCall(rpcCli.createWallet("recipient", descriptors = true))
    handleCall(rpcCli.createWallet("dummy", descriptors = true))
    handleCall(rpcCli.createWallet("alice", descriptors = true))
    handleCall(rpcCli.createWallet("bridge", descriptors = true))
    handleCall(rpcCli.createWallet("watcher", disablePrivateKeys = true, descriptors = true))
//    mineBlocks(1, "testwallet")
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


  /**
   * Create a TX that spends the 50 BTC UTXO into 2 outputs with 25 (to recipient) and 24 (change) BTC respectively.
   *
   * Per the bitcoin.conf file, betch32 is the default address format. This means that the 50 BTC are locked up in a P2WPKH address
   * under the "testwallet".
   *
   * The recipient output will be locked to a new P2WPKH address in the "recipient" wallet.
   * The change output will be locked to a new P2WPKH address in the "testwallet".
   *
   * 1BTC will be used as a fee
   */
  def createTx(): Unit = {
    println("Creating first TX...")
    val utxo = handleCall(rpcCli.listUnspent("testwallet")).get.head
    // The input is the 50 BTC UTXO
    val inputs = Vector(TransactionInput.fromTxidAndVout(utxo.txid, UInt32(utxo.vout)))

    val recipientAddr = BitcoinAddress(handleCall(rpcCli.getNewAddress(Some("recipient"))).get.value)
    val changeAddr = handleCall(rpcCli.getNewAddress(Some("testwallet"))).get
    val outputs = Map(recipientAddr -> 25.bitcoins, changeAddr -> 24.bitcoins)

    val unprovenTx = handleCall(rpcCli.createRawTransaction(inputs, outputs)).get
    val provenTx = handleCall(rpcCli.signRawTransactionWithWallet(unprovenTx, Some("testwallet"))).get.hex
    println("Sending: ")
    println(provenTx)

    handleCall(rpcCli.sendRawTransaction(provenTx, 0)).get
  }

  def run(): Unit = {
    setup()
    checkBalances()

    createTx()
    checkBalances()

    mineBlocks(1)
    checkBalances()
  }
}
