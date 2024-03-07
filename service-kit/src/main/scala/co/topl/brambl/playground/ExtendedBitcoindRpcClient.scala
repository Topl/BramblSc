package co.topl.brambl.playground

import org.bitcoins.commons.jsonmodels.bitcoind.ListSinceBlockResult
import org.bitcoins.commons.serializers.JsonSerializers.listSinceBlockResultReads
import org.bitcoins.core.config.RegTest
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindAuthCredentials, BitcoindInstanceLocal}
import play.api.libs.json.{JsArray, JsBoolean, JsObject, JsString, JsValue}

import java.io.File
import java.net.URI
import java.nio.file.Paths
import scala.concurrent.Future

class ExtendedBitcoindRpcClient(instance: BitcoindInstanceLocal) extends BitcoindRpcClient(instance) {

  private def bitcoindCallRaw(
    command:         String,
    parameters:      List[JsValue] = List.empty,
    uriExtensionOpt: Option[String] = None
  ): Future[JsValue] = {
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
    ).map(res => (res \ "result").result.get.as[JsArray].value.head.as[JsObject].value("success").as[JsBoolean].value)(
      ec
    )

  def listSinceBlockWallet(walletName: String): Future[ListSinceBlockResult] =
    bitcoindCall[ListSinceBlockResult](
      "listsinceblock",
      List.empty,
      uriExtensionOpt = Some(walletExtension(walletName))
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
