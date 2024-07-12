package co.topl.brambl.monitoring

import akka.actor.ActorSystem
import cats.effect.kernel.Resource
import cats.effect.std.Queue
import cats.effect.{IO, Ref}
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
 * @param bitcoind Bitcoind RPC client
 * @param currentTip Reference to the most recent chain tip reported by the monitor
 */
class BitcoinMonitor(
  blockQueue:     Queue[IO, AppliedBitcoinBlock],
  startingBlocks: Vector[AppliedBitcoinBlock],
  bitcoind:       BitcoindRpcClient,
  currentTip:     Ref[IO, Option[AppliedBitcoinBlock]],
  subscriber:     ZMQSubscriber
) {
  subscriber.start()

  /**
   * Return a stream of blocks.
   * @return The infinite stream of blocks. If startingBlocks was provided, they will be at the front of the stream.
   */
  def monitorBlocks(): Stream[IO, BitcoinBlockSync] =
    Stream
      .emits(startingBlocks) ++ Stream.fromQueueUnterminated(blockQueue).through(synchronizeChain).through(flattenChain)

  /**
   * Synchronize the chain.
   * Given a new adopted block, check if it was the result of a reorg. If the new block's parent does not match the monitor's last reported block,
   * we traverse the chain to find the most recent common ancestor. The chain from the last reported block -> ancestor will be added to the monitor queue as "Unapplied" blocks.
   * The chain from the newly adopted block -> ancestor will be added to the monitor queue as "Applied" blocks.
   */
  private def synchronizeChain: Pipe[IO, AppliedBitcoinBlock, Vector[BitcoinBlockSync]] = in =>
    in.evalMap { adoptedBlock =>
      for {
        tip          <- currentTip.get
        updatedChain <-
          // Checking if the new block's parent matches our last reported block
          if (
            tip.isEmpty || tip
              .map(_.block.blockHeader.hashBE)
              .contains(adoptedBlock.block.blockHeader.previousBlockHashBE)
          )
            currentTip.set(Some(adoptedBlock)) >> IO.pure(Vector[BitcoinBlockSync](adoptedBlock))
          else
            currentTip.set(Some(adoptedBlock)) >>
            findCommonAncestor(Vector(UnappliedBitcoinBlock(tip.get.block, tip.get.height)), Vector(adoptedBlock))
              .map(res => res._1 ++ res._2) // report the unapplied first, then the applied.
      } yield updatedChain
    }

  private def flattenChain: Pipe[IO, Vector[BitcoinBlockSync], BitcoinBlockSync] =
    in => in.flatMap(blocks => Stream.emits(blocks))

  /**
   * Find the most recent common ancestor of 2 blocks. Return the traversal path to this ancestor for both blocks
   *
   * Precondition: block1 and block2 are both valid block IDs from the same network. Consequently, they both are guaranteed to share at least 1 common ancestor (genesis block)
   */
  private def findCommonAncestor(
    oldTip: Vector[UnappliedBitcoinBlock],
    newTip: Vector[AppliedBitcoinBlock]
  ): IO[(Vector[UnappliedBitcoinBlock], Vector[AppliedBitcoinBlock])] =
    for {
      updatedNewTip <-
        if (
          newTip.head.height > oldTip.head.height || (newTip.head.height == oldTip.head.height && newTip.head.block.blockHeader.hashBE == newTip.head.block.blockHeader.hashBE)
        ) for {
          newBlock <- IO.fromFuture(IO(bitcoind.getBlockRaw(newTip.head.block.blockHeader.previousBlockHashBE)))
          newBlockHeight <- IO.fromFuture(
            IO(bitcoind.getBlockHeight(newTip.head.block.blockHeader.previousBlockHashBE))
          )
        } yield AppliedBitcoinBlock(newBlock, newBlockHeight.get) +: newTip // height should be 1 less than before
        else IO.pure(newTip)
      updatedOldTip <-
        if (
          newTip.head.height < oldTip.head.height || (newTip.head.height == oldTip.head.height && newTip.head.block.blockHeader.hashBE == newTip.head.block.blockHeader.hashBE)
        ) for {
          oldBlock <- IO.fromFuture(IO(bitcoind.getBlockRaw(oldTip.head.block.blockHeader.previousBlockHashBE)))
          oldBlockHeight <- IO.fromFuture(
            IO(bitcoind.getBlockHeight(oldTip.head.block.blockHeader.previousBlockHashBE))
          )
        } yield UnappliedBitcoinBlock(oldBlock, oldBlockHeight.get) +: oldTip // height should be 1 less than before
        else IO.pure(oldTip)
      res <-
        if (
          updatedNewTip.head.height == updatedOldTip.head.height && updatedNewTip.head.block.blockHeader.hashBE == updatedOldTip.head.block.blockHeader.hashBE
        )
          IO.pure(
            updatedOldTip.tail,
            updatedNewTip.tail
          ) // common ancestor is found. We return .tail so to not duplicate unapply/apply of the ancestor
        else findCommonAncestor(updatedOldTip, updatedNewTip) // keep traversing
    } yield res
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
      proxyParams: Option[Socks5ProxyParams] = None,
      port:        Option[Int] = None,
      rpcPort:     Option[Int] = None
    ): BitcoindRpcClient = BitcoindRpcClient(
      BitcoindInstanceRemote(
        network = network,
        uri = new URI(s"$host:${if (port.isDefined) port.get else network.port}"),
        rpcUri = new URI(s"$host:${if (rpcPort.isDefined) rpcPort.get else network.rpcPort}"),
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

  private def addToQueue(blockQueue: Queue[IO, AppliedBitcoinBlock], bitcoind: BitcoindRpcClient): Block => Unit =
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
    blockQueue: Queue[IO, AppliedBitcoinBlock]
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
  ): Resource[IO, Stream[IO, BitcoinBlockSync]] = {
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
    for {
      blockQueue <- Queue.unbounded[IO, AppliedBitcoinBlock].toResource
      startingBlocks <- IO
        .fromFuture(
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
        .toResource
      currentTip <- Ref.of[IO, Option[AppliedBitcoinBlock]](startingBlocks.lastOption).toResource
      subscriber <- Resource.make(IO(initZmqSubscriber(bitcoind, zmqHost, zmqPort)(blockQueue)))(sub => IO(sub.stop()))
    } yield new BitcoinMonitor(blockQueue, startingBlocks, bitcoind, currentTip, subscriber).monitorBlocks()
  }

}
