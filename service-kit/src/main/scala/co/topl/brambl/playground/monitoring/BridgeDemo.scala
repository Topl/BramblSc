package co.topl.brambl.playground.monitoring

import cats.effect.std.Console
import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.brambl.playground.ScriptBuilder.{PegIn, PegOut}
import co.topl.brambl.playground.monitoring.Models.{BridgeRequest, BridgeResponse}
import co.topl.brambl.playground.monitoring.MonitoringService.ToMonitor
import co.topl.brambl.playground.{handleCall, rpcCli, txBuilder, Bridge, ScriptBuilder}
import co.topl.brambl.utils.Encoding
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import org.bitcoins.core.protocol.BitcoinAddress

import java.io.OutputStream
import java.net.InetSocketAddress

object BridgeDemo extends IOApp {
  println("Initializing bridge...")
  val bridge: Bridge = Bridge()

  implicit class HttpReq(req: HttpExchange) {

    def getParams: Map[String, String] = {
      println(req.getRequestURI.getQuery)
      val pairs = req.getRequestURI.getQuery.split("&")
      pairs.map { p =>
        val kv = p.split("=")
        kv(0) -> kv(1)
      }.toMap
    }

    def parseReq: BridgeRequest = {
      val params = req.getParams
      BridgeRequest(params("hash"), params("bitcoinPk"), params("toplVk"))
    }
  }

  def test(): HttpHandler = (exchange: HttpExchange) => {
    val os: OutputStream = exchange.getResponseBody
    val params = exchange.getParams
    val response = "data: " + params.map(p => s"${p._1} -> ${p._2}").mkString(", ")
    println(response)
    exchange.sendResponseHeaders(200, response.length())
    os.write(response.getBytes())
    os.close()
  }

  def initiateRequest(request: BridgeRequest, scriptBuilder: ScriptBuilder): BridgeResponse = {
    val idx = bridge.toplWallet.walletStateApi.getNextIndicesForFunds("self", "default").unsafeRunSync().get
    println(s"> Using index $idx to generate keys...")
    val toplVk = bridge.toplWallet.getChildVk(idx)
    val btcKey = bridge.btcWallet.getChildSecretKey(idx)
    println("> Bridge generating descriptor...")
    val desc = scriptBuilder.generateDescriptor(btcKey.publicKey.hex, request.hash, request.bitcoinPk)
    val btcAddr = handleCall(rpcCli.deriveOneAddress(bridge.walletName, desc)).get
    println("> watcher importing descriptor...")
    val importDescSuccessful = handleCall(rpcCli.importDescriptor(bridge.btcWallet.watcherName, desc)).get
    println("> watcher importing descriptor successful: " + importDescSuccessful)
    val toplLock = scriptBuilder.generateToplLock(request.toplVk, request.hash, toplVk)
    val toplAddr = txBuilder.lockAddress(toplLock).unsafeRunSync()
    bridge.btcWallet.addWalletEntry(idx, desc, toplAddr)
    bridge.toplWallet.walletStateApi
      .updateWalletState(
        Encoding.encodeToBase58Check(toplLock.getPredicate.toByteArray),
        toplAddr.toBase58(),
        Some("ExtendedEd25519"),
        Some(Encoding.encodeToBase58(toplVk.toByteArray)),
        idx
      )
      .unsafeRunSync()
    BridgeResponse(desc, BitcoinAddress(btcAddr), toplLock, toplAddr)
  }

  def handlePegIn(pegInDescsTransfer: ToMonitor[IO, (String, LockAddress)]): HttpHandler = (exchange: HttpExchange) => {
    print("\n============================" + "Bridge Receives Peg-In Request" + "============================\n")
    val bridgeResp = initiateRequest(exchange.parseReq, PegIn)
    val response = bridgeResp.toJson
    println(s"> Sending response: $response")
    exchange.sendResponseHeaders(200, response.length())
    (
      IO.println("> Adding peg-in desc,addr to monitor...") *>
      pegInDescsTransfer.add((bridgeResp.desc, bridgeResp.toplAddress)) *>
      IO.println("Monitoring if descriptor gets funded...")
    ).unsafeRunSync()
    val os: OutputStream = exchange.getResponseBody
    os.write(response.getBytes())
    os.close()
  }

  def handlePegOut(pegOutLockAddrsTransfer: ToMonitor[IO, (String, LockAddress)]): HttpHandler =
    (exchange: HttpExchange) => {
      print("\n============================" + "Bridge Receives Peg-Out Request" + "============================\n")
      val bridgeResp = initiateRequest(exchange.parseReq, PegOut)
      val response = bridgeResp.toJson
      exchange.sendResponseHeaders(200, response.length())
      println(s"> Sending response: $response")
      (
        IO.println("> Adding peg-out desc,addr to monitor...") *>
        pegOutLockAddrsTransfer.add((bridgeResp.desc, bridgeResp.toplAddress)) *>
        IO.println("Monitoring if adress gets funded...")
      ).unsafeRunSync()
      val os: OutputStream = exchange.getResponseBody
      os.write(response.getBytes())
      os.close()
    }

  // Temporary until Monitor Service is ready
  def notifyOfTbtcClaim(): HttpHandler = (exchange: HttpExchange) => {
    val params = exchange.getParams
    val txId = TransactionId.parseFrom(Encoding.decodeFromHex(params("txId")).toOption.get)
    val addr = AddressCodecs.decodeAddress(params("addr")).toOption.get
    bridge.claimBtc(txId, addr)
    val response = "notified bridge of tbtc claim"
    exchange.sendResponseHeaders(200, response.length())
    val os: OutputStream = exchange.getResponseBody
    os.write(response.getBytes())
    os.close()
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      pegInDescsTransfer      <- ToMonitor.empty[IO, (String, LockAddress)] // Shared state
      pegInLockAddrsClaim     <- ToMonitor.empty[IO, LockAddress] // Shared state
      pegInDescsReclaim       <- ToMonitor.empty[IO, String] // Shared state
      pegOutLockAddrsTransfer <- ToMonitor.empty[IO, (String, LockAddress)] // Shared state
      pegOutDescsClaim        <- ToMonitor.empty[IO, String] // Shared state
      pegOutLockAddrsReclaim  <- ToMonitor.empty[IO, LockAddress] // Shared state
      server = { // Create Mock WS server
        val server = HttpServer.create(new InetSocketAddress(1997), 0)
        server.createContext("/pegin", handlePegIn(pegInDescsTransfer))
        server.createContext("/pegout", handlePegOut(pegOutLockAddrsTransfer))
        server.createContext("/notifyOfTbtcClaim", notifyOfTbtcClaim())
        server.createContext("/test", test())
        server.setExecutor(null) // creates a default executor
        server.start()
        println("Server started on port 1997")
        server
      }
      monitoringService <- MonitoringService(
        bridge,
        pegInDescsTransfer,
        pegInLockAddrsClaim,
        pegInDescsReclaim,
        pegOutLockAddrsTransfer,
        pegOutDescsClaim,
        pegOutLockAddrsReclaim
      ).run().start
      res <- IO.unit.start.foreverM
        .guarantee(
          IO.println("shutting down server") *>
          IO(server.stop(0)) *>
          IO.println("shutting down monitoring") *>
          monitoringService.cancel.start.void
        )
        .as(ExitCode.Success)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res

}
