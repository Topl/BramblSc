package co.topl.brambl.monitoring

import cats.effect.kernel.Async
import cats.effect.std.Queue
import cats.implicits.toFunctorOps
import co.topl.brambl.monitoring.BitcoinMonitor.BitcoinBlock
import fs2.Stream
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.zmq.ZMQSubscriber

import java.net.InetSocketAddress


// TODO: Second pass will retroactively fetch blocks

private class BitcoinMonitor[F[_]: Async](blockQueue: Queue[F, Block]) { // Starting blocks is a stream of blocks
  def monitorBlocks(): Stream[F, BitcoinBlock[F]] = {
    println("monitorBlocks")
    Stream.fromQueueUnterminated(blockQueue).through(s => s.map(block => {
      println(s"Processing block: ${block.blockHeader.hash.hex}")
      BitcoinBlock(block)
    }))
  }
}

object BitcoinMonitor {
  case class BitcoinBlock[F[_]](block: Block){
    def transactions: Stream[F, Transaction] = Stream.emits(block.transactions)
  }
  private def initZmqSocket(host: String = "127.0.0.1", port: Int = 28332) = new InetSocketAddress(host, port)
  private def addToQueue[F[_]: Async](blockQueue: Queue[F, Block]): Block => Unit = (block: Block) => {
    val x = blockQueue.offer(block)
    ()
  }
  def apply[F[_]: Async](startBlock: Option[Block] = None): F[BitcoinMonitor[F]] = for {
    blockQueue <- Queue.unbounded[F, Block]
    subscriber = new ZMQSubscriber(initZmqSocket(), None, None, None, Some(addToQueue(blockQueue)))
    _ = subscriber.start()
  } yield new BitcoinMonitor[F](blockQueue)
}
