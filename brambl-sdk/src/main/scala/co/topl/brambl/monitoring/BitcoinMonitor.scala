package co.topl.brambl.monitoring

import akka.actor.ActorSystem
import cats.effect.IO
import cats.effect.std.Queue
import co.topl.brambl.monitoring.BitcoinMonitor.{AppliedBitcoinBlock, BitcoinBlockSync, UnappliedBitcoinBlock}
import fs2.{Pipe, Stream}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindAuthCredentials, BitcoindInstanceLocal, BitcoindInstanceRemote}
import org.bitcoins.tor.Socks5ProxyParams
import org.bitcoins.zmq.ZMQSubscriber

import java.io.File
import java.net.{InetSocketAddress, URI}
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/**
 * Class to monitor incoming bitcoin blocks via a queue.
 *
 * @note This monitor does not do any filtering or self-healing on the block stream if an outage of the bitcoin node or
 *       monitoring client occurs. For example, if a bitcoin node restart causes the entire chain to get re-applied, then
 *       the monitor will report all blocks again. In other words, duplicate blocks may appear in the stream.
 *
 * @param blockQueue The queue in which new blocks will be added to
 * @param startingBlocks Past blocks that should be reported.
 * @param initZmqSubscriber Zmq Subscriber Initializer
 */
class BitcoinMonitor(
  blockQueue:        Queue[IO, BitcoinBlockSync],
  startingBlocks:    Vector[BitcoinBlockSync],
  initZmqSubscriber: Queue[IO, BitcoinBlockSync] => ZMQSubscriber,
  bitcoind: BitcoindRpcClient
) {
  val subscriber: ZMQSubscriber = initZmqSubscriber(blockQueue)
  subscriber.start()

  // The last live block that was reported by the monitor. Used to detect reorgs
  var currentTip: Option[DoubleSha256DigestBE] = startingBlocks.lastOption.map(_.block.blockHeader.hashBE)

  /**
   * Return a stream of blocks.
   * @return The infinite stream of blocks. If startingBlocks was provided, they will be at the front of the stream.
   */
  def monitorBlocks(): Stream[IO, BitcoinBlockSync] =
    Stream.emits(startingBlocks) ++ Stream.fromQueueUnterminated(blockQueue).through(synchronizeChain)

  /**
   *
   * @return
   */
  private def synchronizeChain: Pipe[IO, BitcoinBlockSync, BitcoinBlockSync] = in => in.flatMap { adoptedBlock =>
    if(currentTip.isEmpty || currentTip.contains(adoptedBlock.block.blockHeader.previousBlockHashBE)) {
      currentTip = Some(adoptedBlock.block.blockHeader.hashBE)
      Stream.emits(Vector(adoptedBlock))
    } else {
      val commonAncestor = findCommonAncestor(currentTip.get, adoptedBlock.block.blockHeader.previousBlockHashBE)
      val unappliedChain = buildSyncChain(commonAncestor, currentTip.get, UnappliedBitcoinBlock)
      val appliedChain = buildSyncChain(commonAncestor, adoptedBlock.block.blockHeader.previousBlockHashBE, AppliedBitcoinBlock) :+ adoptedBlock
      Stream.emits(unappliedChain ++ appliedChain) // report the unapplied first, then the applied.
    }
  }

  /**
   * Find the most recent common ancestor of 2 blocks
   *
   * Precondition: block1 and block2 are both valid block IDs from the same network. Consequently, they both are guaranteed to share at least 1 common ancestor (genesis block)
   * */
  private def findCommonAncestor(block1: DoubleSha256DigestBE, block2: DoubleSha256DigestBE): DoubleSha256DigestBE = ???

  private def buildSyncChain(start: DoubleSha256DigestBE, end: DoubleSha256DigestBE, constructor: (Block, Int) => BitcoinBlockSync): Vector[BitcoinBlockSync] =
    buildSyncChainRawBlocks(end, start).map(block => {
      val h = Await.result(bitcoind.getBlockHeight(block.blockHeader.hashBE), 5.seconds).get // getBlockHeight hard-codes Some(_)
      constructor(block, h)
    })

  @tailrec
  private def buildSyncChainRawBlocks(curId: DoubleSha256DigestBE, firstId: DoubleSha256DigestBE, accBlocks: Vector[Block] = Vector.empty): Vector[Block] = {
    val curBlock = Await.result(bitcoind.getBlockRaw(curId.flip), 5.seconds)
    if(curId == firstId)
      curBlock +: accBlocks
    else
      buildSyncChainRawBlocks(curBlock.blockHeader.previousBlockHashBE, firstId, curBlock +: accBlocks)
  }

  /**
   * Stop monitorings
   */
  def stop(): Unit = subscriber.stop()
}

object BitcoinMonitor {

  object Bitcoind {
    implicit val system: ActorSystem = ActorSystem("System")

    /**
     * Connection to the bitcoind RPC server instance
     * @param network Parameters of a given network to be used
     * @param host The host to connect to the bitcoind instance
     * @param credentials rpc credentials
     * @param binary the bitcoind executable
     * @return
     */
    def localConnection(
      network:     NetworkParameters,
      host:        String,
      credentials: BitcoindAuthCredentials,
      binary:      File
    ): BitcoindRpcClient = BitcoindRpcClient(
      BitcoindInstanceLocal(
        network = network,
        uri = new URI(s"$host:${network.port}"),
        rpcUri = new URI(s"$host:${network.rpcPort}"),
        authCredentials = credentials,
        binary = binary
      )
    )

    /**
     * Connection to the bitcoind RPC server instance
     *
     * @param network     Parameters of a given network to be used
     * @param host        The host to connect to the bitcoind instance
     * @param credentials rpc credentials
     * @param proxyParams proxy parameters
     * @return
     */
    def remoteConnection(
      network:     NetworkParameters,
      host:        String,
      credentials: BitcoindAuthCredentials,
      proxyParams: Option[Socks5ProxyParams] = None
    ): BitcoindRpcClient = BitcoindRpcClient(
      BitcoindInstanceRemote(
        network = network,
        uri = new URI(s"$host:${network.port}"),
        rpcUri = new URI(s"$host:${network.rpcPort}"),
        authCredentials = credentials,
        proxyParams = proxyParams
      )
    )
  }

  /**
   * A wrapper for a bitcoin Block update.
   */
  trait BitcoinBlockSync {
    val block: Block
    val height: Int

    def transactions[F[_]]: Stream[F, Transaction] = Stream.emits(block.transactions)
  }

  // Represents a new block applied to the chain tip
  case class AppliedBitcoinBlock(block: Block, height: Int) extends BitcoinBlockSync

  // Represents an existing block that has been unapplied from the chain tip
  case class UnappliedBitcoinBlock(block: Block, height: Int) extends BitcoinBlockSync

  private def addToQueue(blockQueue: Queue[IO, BitcoinBlockSync], bitcoind: BitcoindRpcClient): Block => Unit =
    (block: Block) => {
      import cats.effect.unsafe.implicits.global

      (for {
        height <- IO.fromFuture(IO(bitcoind.getBlockHeight(block.blockHeader.hashBE)))
        res    <- blockQueue.offer(AppliedBitcoinBlock(block, height.get)) // getBlockHeight hard-codes Some(_)
      } yield res).unsafeRunSync()
    }

  /**
   * Initialize a ZeroMQ subscriber that adds new blocks to a queue
   * @param bitcoind The bitcoind instance to query additional data
   * @param host The host in which the ZmqSubscriber will be connected to
   * @param port The port in which the ZmqSubscriber will be connected to
   * @param blockQueue The queue to add new blocks to
   * @return An initialized ZMQSubcriber instance
   */
  def initZmqSubscriber(bitcoind: BitcoindRpcClient, host: String, port: Int)(
    blockQueue: Queue[IO, BitcoinBlockSync]
  ): ZMQSubscriber =
    new ZMQSubscriber(new InetSocketAddress(host, port), None, None, None, Some(addToQueue(blockQueue, bitcoind)))

  /**
   * Initialize and return a BitcoinMonitor instance.
   * @param bitcoind The bitcoind instance to monitor
   * @param startBlock The hash of a past block. Used to retroactively report blocks. The bitcoin monitor will report all blocks starting at this block.
   * @param zmqHost The host in which the underlying ZmqSubscriber will be connected to. This is used to capture newly minted blocks.
   * @param zmqPort The port in which the underlying ZmqSubscriber will be connected to. This is used to capture newly minted blocks.
   * @return An instance of a BitcoinMonitor
   */
  def apply(
    bitcoind:   BitcoindRpcClient,
    startBlock: Option[DoubleSha256DigestBE] = None,
    zmqHost:    String = "127.0.0.1",
    zmqPort:    Int = 28332
  ): IO[BitcoinMonitor] = {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    @tailrec
    def getBlockHashes(
      curHash:    Option[DoubleSha256DigestBE],
      prevHashes: Vector[DoubleSha256DigestBE] = Vector.empty[DoubleSha256DigestBE]
    ): Vector[DoubleSha256DigestBE] = curHash match {
      case Some(blockHash) =>
        val blockRes = Await.result(bitcoind.getBlock(blockHash), 5.seconds)
        getBlockHashes(blockRes.nextblockhash, prevHashes.appended(blockHash))
      case None => prevHashes
    }

    val existingHashes = getBlockHashes(startBlock)
    println("Retroactively fetching blocks:")
    existingHashes.foreach(h => println(h.hex))
    for {
      blockQueue <- Queue.unbounded[IO, BitcoinBlockSync]
      startingBlocks <- IO.fromFuture(
        IO(
          Future.sequence(
            existingHashes.map(hash =>
              for {
                b <- bitcoind.getBlockRaw(hash)
                h <- bitcoind.getBlockHeight(hash)
              } yield AppliedBitcoinBlock(b, h.get)
            ) // .get won't cause issue since the getBlockHeight hardcodes it as Some(_)
          )
        )
      )
    } yield new BitcoinMonitor(blockQueue, startingBlocks, initZmqSubscriber(bitcoind, zmqHost, zmqPort), bitcoind)
  }

}
