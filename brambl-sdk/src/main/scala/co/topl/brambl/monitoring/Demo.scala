package co.topl.brambl.monitoring

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.monitoring.BitcoinMonitor.BitcoinBlock
import org.bitcoins.core.config.RegTest
import org.bitcoins.crypto.DoubleSha256DigestBE
import fs2.{Pipe, Stream}

import java.io.File
import java.nio.file.Paths

object Demo extends App {

  // This is the path to your bitcoind executable
  val BitcoindPath = Paths.get("C:", "Program Files", "Bitcoin", "daemon", "bitcoind.exe")

  // This (username, password) pair comes from 'rpcuser' and 'rpcpassword' in your bitcoin.conf file
  val bitcoind = BitcoinMonitor.Bitcoind
    .connection(RegTest, "http://localhost", "diadem", "NsLbSu6PQc4vYlz", new File(BitcoindPath.toString))

  val startBlock = Some(
    DoubleSha256DigestBE
      // A block that has already passed
      .fromHex("75d45e17386863909bcd297c503e270f9ffe77b895ba6d6453c2dd1badbbfcca")
  )

  def myPipe: Pipe[IO, BitcoinBlock, String] = _.flatMap { block =>
    Stream(s"Block: ${block.block.blockHeader.hash.flip.hex}") ++ block.transactions.map(tx =>
      s"   - tx:${tx.txId.flip.hex}"
    )
  }

  val bitcoinMonitor = BitcoinMonitor(bitcoind, startBlock).map(_.monitorBlocks().through(myPipe).map(println))

  val monitoring = for {
    bitcoinStream <- bitcoinMonitor
    doMonitor     <- bitcoinStream.compile.drain.start *> IO.unit.foreverM
  } yield doMonitor

  monitoring.unsafeRunSync()
}
