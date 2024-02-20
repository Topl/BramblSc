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
import co.topl.brambl.playground.{Bridge, ScriptBuilder, handleCall, rpcCli, txBuilder}
import co.topl.brambl.utils.Encoding
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import java.io.OutputStream
import java.net.InetSocketAddress

object BridgeDemo extends IOApp {
  println("Initializing bridge...")
  val bridge: Bridge = Bridge()

  implicit class HttpReq(req: HttpExchange) {
    def getParams: Map[String, String] = {
      println(req.getRequestURI.getQuery)
      val pairs = req.getRequestURI.getQuery.split("&")
      pairs.map(p => {
        val kv = p.split("=")
        kv(0) -> kv(1)
      }).toMap
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
    BridgeResponse(desc, toplLock, toplAddr)
  }

  def handlePegIn(pegInLockAddrs: ToMonitor[IO, LockAddress], pegInDescs: ToMonitor[IO, String]): HttpHandler = (exchange: HttpExchange) => {
    print("\n============================" + "Bridge Receives Peg-In Request" + "============================\n")
    val bridgeResp = initiateRequest(exchange.parseReq, PegIn)
    val response = bridgeResp.toJson
    println(s"> Sending response: $response")
    exchange.sendResponseHeaders(200, response.length())
    (
      IO.println("> Adding peg-in addresses to monitor...") *>
      Seq(
        pegInLockAddrs.add(bridgeResp.toplAddress),
        pegInDescs.add(bridgeResp.desc)
      ).parSequence *>
      IO.println("Addresses Added to monitor")
    ).unsafeRunSync()
    val os: OutputStream = exchange.getResponseBody
    os.write(response.getBytes())
    os.close()
  }
  def handlePegOut(pegInLockAddrs: ToMonitor[IO, LockAddress], pegInDescs: ToMonitor[IO, String]): HttpHandler = (exchange: HttpExchange) => {
    print("\n============================" + "Bridge Receives Peg-Out Request" + "============================\n")
    val bridgeResp = initiateRequest(exchange.parseReq, PegOut)
    val response = bridgeResp.toJson
    exchange.sendResponseHeaders(200, response.length())
    println(s"> Sending response: $response")
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

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      pegInLockAddrs <- ToMonitor.empty[IO, LockAddress] // Shared state
      pegInDescsTransfer <- ToMonitor.empty[IO, String] // Shared state
      pegInDescsReclaim <- ToMonitor.empty[IO, String] // Shared state
      pegOutDescs <- ToMonitor.empty[IO, String] // Shared state
      pegOutLockAddrsTransfer <- ToMonitor.empty[IO, LockAddress] // Shared state
      pegOutLockAddrsReclaim <- ToMonitor.empty[IO, LockAddress] // Shared state
      server = { // Create Mock WS server
        val server = HttpServer.create(new InetSocketAddress(1997), 0)
        server.createContext("/pegin", handlePegIn(pegInLockAddrs, pegInDescsTransfer))
        server.createContext("/pegout", handlePegOut(pegInLockAddrs, pegInDescsTransfer))
        server.createContext("/notifyOfTbtcClaim", notifyOfTbtcClaim())
        server.createContext("/test", test())
        server.setExecutor(null) // creates a default executor
        server.start()
        println("Server started on port 1997")
        server
      }
      monitoringService <- MonitoringService(
        bridge,
        pegInLockAddrs,
        pegInDescsTransfer,
        pegInDescsReclaim,
        pegOutDescs,
        pegOutLockAddrsTransfer,
        pegOutLockAddrsReclaim
      ).run().start
      res <- IO.unit
        .start.foreverM
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

}
