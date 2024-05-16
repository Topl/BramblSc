package co.topl.brambl.monitoring

import cats.effect.IO
import org.bitcoins.core.config.RegTest
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindAuthCredentials

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class BitcoinMonitorTest extends munit.CatsEffectSuite {
  val TestWallet = "test"
  val NumBlocks = 5
  val bitcoind = new Fixture[BitcoindRpcClient]("bitcoin-monitor") {
    val credentials = BitcoindAuthCredentials.PasswordBased(TestWallet, TestWallet)

    val bitcoindInstance: BitcoindRpcClient = BitcoinMonitor.Bitcoind.remoteConnection(RegTest, "http://localhost", credentials)
    def apply() = bitcoindInstance

    override def beforeAll(): Unit = {
      println("beforeall")
      Await.result(
        bitcoindInstance.createWallet(TestWallet, descriptors = true)
        .recover(_ => ()), // In case wallet already exists
        5.seconds)
      println("wallet initialized")
    }
  }
  override def munitFixtures = List(bitcoind)

  test("Monitor only new blocks") {
    val bitcoindInstance = bitcoind()
    assertIO(for {
      monitor <- BitcoinMonitor(bitcoindInstance)
      blockStream = monitor.monitorBlocks()
      mintBlocks <- IO.fromFuture(IO(bitcoindInstance.getNewAddress(Some(TestWallet)).flatMap(bitcoindInstance.generateToAddress(NumBlocks, _))))
      // After 0.5 second to allow the minted blocks to occur on the bitcoin instance
      blocks <- blockStream.interruptAfter(500.millis).compile.toList
      _ = monitor.stop()
    } yield blocks.map(_.block.blockHeader.hashBE).toVector == mintBlocks && blocks.map(_.height) == mintBlocks.zipWithIndex.map(_._2 + 1),
      true
    )
  }
  test("Monitor new blocks and report existing blocks") {
    val bitcoindInstance = bitcoind()
    val existingBlocks = Await.result(bitcoindInstance.getNewAddress(Some(TestWallet)).flatMap(bitcoindInstance.generateToAddress(NumBlocks, _)), 5.seconds)
    // Allow 0.5 second to allow the minted blocks to occur on the bitcoin instance
    Thread.sleep(500)
    assertIO(for {
      monitor <- BitcoinMonitor(bitcoindInstance, Some(existingBlocks.head))
      blockStream = monitor.monitorBlocks()
      mintBlocks <- IO.fromFuture(IO(bitcoindInstance.getNewAddress(Some(TestWallet)).flatMap(bitcoindInstance.generateToAddress(NumBlocks, _))))
      blocks <- blockStream.interruptAfter(1.seconds).compile.toList
      _ = monitor.stop()
    } yield {
      println(existingBlocks)
      println(mintBlocks)
      println(blocks.map(_.block.blockHeader.hashBE).toVector)
      val expectedBlocks = existingBlocks ++ mintBlocks
      val startingHeight = blocks.head.height
      blocks.map(_.block.blockHeader.hashBE).toVector == expectedBlocks && blocks.map(_.height) == expectedBlocks.zipWithIndex.map(_._2 + startingHeight)
    },
      true
    )
  }
}
