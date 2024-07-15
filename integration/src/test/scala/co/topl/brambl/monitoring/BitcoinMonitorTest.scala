package co.topl.brambl.monitoring

import cats.effect.IO
import co.topl.brambl.monitoring.BitcoinMonitor.AppliedBitcoinBlock
import org.bitcoins.core.config.RegTest
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindAuthCredentials

import java.net.URI
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class BitcoinMonitorTest extends munit.CatsEffectSuite {
  val TestWallet = "test"
  val NumBlocks = 5
  val bitcoind = new Fixture[BitcoindRpcClient]("bitcoin-monitor") {
    val credentials = BitcoindAuthCredentials.PasswordBased(TestWallet, TestWallet)

    val bitcoindInstance: BitcoindRpcClient = BitcoinMonitor.Bitcoind.remoteConnection(RegTest, "http://localhost", credentials)
    def apply() = bitcoindInstance

    val bitcoindInstance2: BitcoindRpcClient = BitcoinMonitor.Bitcoind.remoteConnection(
      RegTest,
      "http://localhost",
      credentials,
      port = Some(12224),
      rpcPort = Some(12223)
    )
    val bitcoindInstance2Uri = new URI("http://host.docker.internal:12224")

    override def beforeAll(): Unit = {
      println("beforeall")
      Await.result(
        bitcoindInstance.createWallet(TestWallet, descriptors = true)
        .recover(_ => ()), // In case wallet already exists
        5.seconds)
      Await.result(
        bitcoindInstance2.createWallet(TestWallet, descriptors = true)
          .recover(_ => ()), // In case wallet already exists
        5.seconds)
      println("wallet initialized")
    }
  }
  override def munitFixtures = List(bitcoind)

  test("Monitor blocks with a reorg") {
    val bitcoindInstance = bitcoind()
    val node2Instance = bitcoind.bitcoindInstance2
    assertIO(
      BitcoinMonitor(bitcoindInstance).use(blockStream => {
        for {
          node1MintBlocks <- IO.fromFuture(IO(bitcoindInstance.getNewAddress(walletName = TestWallet).flatMap(bitcoindInstance.generateToAddress(1, _))))
          node2MintBlocks <- IO.fromFuture(IO(node2Instance.getNewAddress(walletName = TestWallet).flatMap(node2Instance.generateToAddress(2, _))))
          _ <- connectBitcoinNodes("bitcoind", "bitcoind2", TestWallet).start.andWait(5.seconds)
          additionalMintBlocks <- IO.fromFuture(IO(node2Instance.getNewAddress(walletName = TestWallet).flatMap(node2Instance.generateToAddress(1, _))))
          blocks <- blockStream.interruptAfter(5.seconds).compile.toList
        } yield {
          case class BitcoinSyncLite(height: Int, hash: DoubleSha256DigestBE, isApplied: Boolean)
          val startingHeight = blocks.head.height
          val expectedBlocks = Seq(
            BitcoinSyncLite(startingHeight, node1MintBlocks.head, isApplied = true),
            BitcoinSyncLite(startingHeight, node1MintBlocks.head, isApplied = false),
            BitcoinSyncLite(startingHeight, node2MintBlocks.head, isApplied = true),
            BitcoinSyncLite(startingHeight + 1, node2MintBlocks(1), isApplied = true),
            BitcoinSyncLite(startingHeight + 2, additionalMintBlocks.head, isApplied = true),
          )
          val testBlocks = blocks.map(b => BitcoinSyncLite(b.height, b.block.blockHeader.hashBE, b.isInstanceOf[AppliedBitcoinBlock]))
          expectedBlocks == testBlocks
        }
      }), true
    )

  }

  test("Monitor only new blocks") {
    val bitcoindInstance = bitcoind()
    assertIO(
      BitcoinMonitor(bitcoindInstance).use(blockStream => {
        for {
          mintBlocks <- IO.fromFuture(IO(bitcoindInstance.getNewAddress(walletName = TestWallet).flatMap(bitcoindInstance.generateToAddress(NumBlocks, _))))
          // After 0.5 second to allow the minted blocks to occur on the bitcoin instance
          blocks <- blockStream.interruptAfter(500.millis).compile.toList
        } yield {
          val startingHeight = blocks.head.height
          blocks.map(_.block.blockHeader.hashBE).toVector == mintBlocks && blocks.map(_.height) == mintBlocks.zipWithIndex.map(_._2 + startingHeight)
        }
      }),
      true
    )
  }
  test("Monitor new blocks and report existing blocks") {
    val bitcoindInstance = bitcoind()
    val existingBlocks = Await.result(bitcoindInstance.getNewAddress(walletName = TestWallet).flatMap(bitcoindInstance.generateToAddress(NumBlocks, _)), 5.seconds)
    // Allow 1 second to allow the minted blocks to occur on the bitcoin instance
    Thread.sleep(1000)
    assertIO(
      BitcoinMonitor(bitcoindInstance, Some(existingBlocks.head)).use(blockStream => {
        for {
          mintBlocks <- IO.fromFuture(IO(bitcoindInstance.getNewAddress(walletName = TestWallet).flatMap(bitcoindInstance.generateToAddress(NumBlocks, _))))
          blocks <- blockStream.interruptAfter(1.seconds).compile.toList
        } yield {
          val expectedBlocks = existingBlocks ++ mintBlocks
          val startingHeight = blocks.head.height
          blocks.map(_.block.blockHeader.hashBE).toVector == expectedBlocks && blocks.map(_.height) == expectedBlocks.zipWithIndex.map(_._2 + startingHeight)
        }
      })
      ,
      true
    )
  }
}
