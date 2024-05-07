package co.topl.brambl.monitoring

import cats.effect.IO
import cats.implicits.{catsSyntaxParallelSequence1, toTraverseOps}
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
      case Applied(blockId)   => getFullBlock(blockId).map(block => AppliedBifrostBlock(block, blockId))
      case Unapplied(blockId) => getFullBlock(blockId).map(block => UnappliedBifrostBlock(block, blockId))
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
    val id: BlockId
    def transactions[F[_]]: Stream[F, IoTransaction] = Stream.emits(block.transactions)
  }

  // Represents a new block applied to the chain tip
  case class AppliedBifrostBlock(block: FullBlockBody, id: BlockId) extends BifrostBlockSync
  // Represents an existing block that has been unapplied from the chain tip
  case class UnappliedBifrostBlock(block: FullBlockBody, id: BlockId) extends BifrostBlockSync

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
    def getBlockIds(startHeight: Option[Long], tipHeight: Option[Long]): IO[Vector[BlockId]] =
      (startHeight, tipHeight) match {
        case (Some(start), Some(tip)) if (start >= 1 && tip > start) =>
          (for (curHeight <- start to tip)
            yield
            // For all blocks from starting Height to current tip height, fetch blockIds
            bifrostQuery.blockByHeight(curHeight).map(_.map(_._1)).map(_.toList)).toVector.parSequence.map(_.flatten)
        case _ => IO.pure(Vector.empty)
      }
    for {
      // The height of the startBlock
      startBlockHeight <- startBlock.map(bId => bifrostQuery.blockById(bId)).sequence.map(_.flatten.map(_._2.height))
      // the height of the chain tip
      tipHeight        <- bifrostQuery.blockByDepth(1).map(_.map(_._2.height))
      startingBlockIds <- getBlockIds(startBlockHeight, tipHeight)
      startingBlocks <- startingBlockIds
        .map(bId => getFullBlock(bId).map(block => AppliedBifrostBlock(block, bId)))
        .sequence
      updates <- bifrostQuery.synchronizationTraversal()
    } yield new BifrostMonitor(updates, getFullBlock, startingBlocks)
  }

}
