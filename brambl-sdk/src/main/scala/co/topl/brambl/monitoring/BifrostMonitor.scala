package co.topl.brambl.monitoring

import cats.effect.IO
import co.topl.brambl.dataApi.BifrostQueryAlgebra
import co.topl.brambl.display.blockIdDisplay.display
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.monitoring.BifrostMonitor.{AppliedBifrostBlock, BifrostBlockSync, UnappliedBifrostBlock}
import co.topl.consensus.models.BlockId
import co.topl.node.models.FullBlockBody
import co.topl.node.services.SynchronizationTraversalRes
import co.topl.node.services.SynchronizationTraversalRes.Status.{Applied, Unapplied}
import fs2.Stream

/**
 * Class to monitor incoming bifrost blocks via an iterator.
 * @param blockIterator The iterator in which block changes will be added to
 * @param startingBlocks Past blocks that should be reported.
 */
class BifrostMonitor(
  blockIterator:  Iterator[SynchronizationTraversalRes],
  getFullBlock:   BlockId => IO[FullBlockBody],
  startingBlocks: Vector[BifrostBlockSync]
) {

  def pipe(in: Stream[IO, SynchronizationTraversalRes]): Stream[IO, BifrostBlockSync] = in.evalMap(sync =>
    sync.status match {
      case Applied(blockId)   => getFullBlock(blockId).map(AppliedBifrostBlock)
      case Unapplied(blockId) => getFullBlock(blockId).map(UnappliedBifrostBlock)
    }
  )

  /**
   * Return a stream of block updates.
   * @return The infinite stream of block updatess. If startingBlocks was provided, they will be at the front of the stream.
   */
  def monitorBlocks(): Stream[IO, BifrostBlockSync] = Stream.emits(startingBlocks) ++
    Stream.fromIterator[IO](blockIterator, 1).through(pipe)

}

object BifrostMonitor {

  /**
   * A wrapper for a Bifrost Block Sync update.
   */
  trait BifrostBlockSync {
    // The bifrost Block being wrapped. This represents either an Applied block or an Unapplied block
    val block: FullBlockBody
    def transactions[F[_]]: Stream[F, IoTransaction] = Stream.emits(block.transactions)
  }

  // Represents a new block applied to the chain tip
  case class AppliedBifrostBlock(block: FullBlockBody) extends BifrostBlockSync
  // Represents an existing block that has been unapplied from the chain tip
  case class UnappliedBifrostBlock(block: FullBlockBody) extends BifrostBlockSync

  /**
   * Initialize and return a BifrostMonitor instance.
   * @param bifrostQuery The bifrost query api to retrieve updates from
   * @param startBlock The blockId of a past block. Used to retroactively report blocks. The bifrost monitor will report all blocks starting at this block.
   * @return An instance of a BifrostMonitor
   */
  def apply(
    bifrostQuery: BifrostQueryAlgebra[IO],
    startBlock:   Option[BlockId] = None
  ): IO[BifrostMonitor] = {
    def getFullBlock(blockId: BlockId): IO[FullBlockBody] = for {
      block <- bifrostQuery.blockById(blockId)
    } yield block match {
      case None                 => throw new Exception(s"Unable to query block ${display(blockId)}")
      case Some((_, _, _, txs)) => FullBlockBody(txs)
    }
    for {
      updates <- bifrostQuery.synchronizationTraversal()
    } yield new BifrostMonitor(updates, getFullBlock, Vector.empty)
  }

}
