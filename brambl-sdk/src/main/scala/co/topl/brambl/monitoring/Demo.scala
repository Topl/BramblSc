package co.topl.brambl.monitoring

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.monitoring.BitcoinMonitor.BitcoinBlock
import fs2.{Pipe, Stream}

object Demo extends App {
  def myPipe: Pipe[IO, BitcoinBlock[IO], String] = _.flatMap(block => {
    Stream(s"Block: ${block.block.blockHeader.hash.hex}") ++ block.transactions.map(tx => s"   - tx:${tx.txId.flip.hex}")
  })

  val bitcoinMonitor = BitcoinMonitor().map(_.monitorBlocks().through(myPipe).map(println))
  val monitoring = for {
    bitcoinStream <- bitcoinMonitor
    doMonitor <- bitcoinStream.compile.drain.start *> IO.unit.foreverM
  } yield doMonitor

  monitoring.unsafeRunSync()

}


