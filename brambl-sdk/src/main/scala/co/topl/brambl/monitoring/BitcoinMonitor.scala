package co.topl.brambl.monitoring

import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import co.topl.brambl.monitoring.BitcoinMonitor.{BitcoinBlock, initZmqSubscriber}
import fs2.Stream
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.zmq.ZMQSubscriber

import java.net.InetSocketAddress


// TODO: Second pass will retroactively fetch blocks
    // Use ChainApi.getHeadersBetween(from, to) (from provided from bridge, to is most recent)
    // With the headers, query for the blocks themselves (???) and add to stream
// TODO: Try to use Type parameter F

class BitcoinMonitor(val blockQueue: Queue[IO, Block]) {
  val subscriber: ZMQSubscriber = initZmqSubscriber(blockQueue)
  subscriber.start()
  def monitorBlocks(): Stream[IO, BitcoinBlock[IO]] =
    Stream.fromQueueUnterminated(blockQueue).through(s => s.map(block => BitcoinBlock(block)))
}

object BitcoinMonitor {
  case class BitcoinBlock[F[_]](block: Block){
    def transactions: Stream[F, Transaction] = Stream.emits(block.transactions)
  }
  private def addToQueue(blockQueue: Queue[IO, Block]): Block => Unit = (block: Block) =>
    blockQueue.offer(block).unsafeRunSync()
  def initZmqSubscriber(blockQueue: Queue[IO, Block], host: String = "127.0.0.1", port: Int = 28332): ZMQSubscriber =
    new ZMQSubscriber(new InetSocketAddress(host, port), None, None, None, Some(addToQueue(blockQueue)))
  def apply(startBlock: Option[Block] = None): IO[BitcoinMonitor] =
    Queue.unbounded[IO, Block].map(q => new BitcoinMonitor(q))
}
