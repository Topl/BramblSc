package co.topl.brambl.monitoring

import akka.actor.ActorSystem
import cats.effect.IO
import cats.effect.std.Queue
import co.topl.brambl.monitoring.BitcoinMonitor.{initZmqSubscriber, BitcoinBlock}
import fs2.Stream
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindAuthCredentials, BitcoindInstanceLocal}
import org.bitcoins.zmq.ZMQSubscriber

import java.io.File
import java.net.{InetSocketAddress, URI}
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/**
 * Class to monitor incoming bitcoin blocks via a queue.
 * @param blockQueue The queue in which new blocks will be added to
 * @param startingBlocks Past blocks that should be reported.
 * @param zmqHost The host where the monitor will be subscribed to
 * @param zmqPort The port where the monitor will be subscribed to
 */
class BitcoinMonitor(
  blockQueue:     Queue[IO, BitcoinBlock],
  startingBlocks: Vector[BitcoinBlock],
  zmqHost:        String,
  zmqPort:        Int
) {
  val subscriber: ZMQSubscriber = initZmqSubscriber(blockQueue, zmqHost, zmqPort)
  subscriber.start()

  /**
   * Return a stream of blocks.
   * @return The infinite stream of blocks. If startingBlocks was provided, they will be at the front of the stream.
   */
  def monitorBlocks(): Stream[IO, BitcoinBlock] =
    Stream.emits(startingBlocks) ++ Stream.fromQueueUnterminated(blockQueue)
}

object BitcoinMonitor {

  object Bitcoind {
    implicit val system: ActorSystem = ActorSystem("System")

    /**
     * Connection to the bitcoind RPC server instance
     * @param network Parameters of a given network to be used
     * @param host The host to connect to the bitcoind instance
     * @param rpcUser rpc username
     * @param rpcPassword rpc password
     * @param binary the bitcoind executable
     * @return
     */
    def connection(
      network:     NetworkParameters,
      host:        String,
      rpcUser:     String,
      rpcPassword: String,
      binary:      File
    ): BitcoindInstanceLocal = BitcoindInstanceLocal(
      network = network,
      uri = new URI(s"$host:${network.port}"),
      rpcUri = new URI(s"$host:${network.rpcPort}"),
      authCredentials = BitcoindAuthCredentials.PasswordBased(rpcUser, rpcPassword),
      binary = binary
    )
  }

  /**
   * A wrapper for a bitcoin Block.
   * @param block The bitcoin Block being wrapped
   */
  case class BitcoinBlock(block: Block) {
    def transactions[F[_]]: Stream[F, Transaction] = Stream.emits(block.transactions)
  }

  private def addToQueue(blockQueue: Queue[IO, BitcoinBlock]): Block => Unit = (block: Block) => {
    import cats.effect.unsafe.implicits.global
    // Using unsafeRunSync since integrating with non-functional code
    blockQueue.offer(BitcoinBlock(block)).unsafeRunSync()
  }

  /**
   * Initialize a ZeroMQ subscriber that adds new blocks to a queue
   * @param blockQueue The queue to add new blocks to
   * @param host The host in which the ZmqSubscriber will be connected to
   * @param port The port in which the ZmqSubscriber will be connected to
   * @return An initialized ZMQSubcriber instance
   */
  def initZmqSubscriber(blockQueue: Queue[IO, BitcoinBlock], host: String, port: Int): ZMQSubscriber =
    new ZMQSubscriber(new InetSocketAddress(host, port), None, None, None, Some(addToQueue(blockQueue)))

  /**
   * Initialize and return a BitcoinMonitor instance.
   * @param bitcoindInstance The local bitcoind instance to monitor
   * @param startBlock The hash of a past block. Used to retroactively report blocks. The bitcoin monitor will report all blocks starting at this block.
   * @param zmqHost The host in which the underlying ZmqSubscriber will be connected to. This is used to capture newly minted blocks.
   * @param zmqPort The port in which the underlying ZmqSubscriber will be connected to. This is used to capture newly minted blocks.
   * @return An instance of a BitcoinMonitor
   */
  def apply(
    bitcoindInstance: BitcoindInstanceLocal,
    startBlock:       Option[DoubleSha256DigestBE] = None,
    zmqHost:          String = "127.0.0.1",
    zmqPort:          Int = 28332
  ): IO[BitcoinMonitor] = {
    import cats.effect.unsafe.implicits.global
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val bitcoind = BitcoindRpcClient(bitcoindInstance)

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
      blockQueue <- Queue.unbounded[IO, BitcoinBlock]
      startingBlocks <- IO.fromFuture(
        IO(
          Future.sequence(
            existingHashes.map(hash => bitcoind.getBlockRaw(hash).map(b => BitcoinBlock(b))(ec))
          )
        )
      )
    } yield new BitcoinMonitor(blockQueue, startingBlocks, zmqHost, zmqPort)
  }

}
