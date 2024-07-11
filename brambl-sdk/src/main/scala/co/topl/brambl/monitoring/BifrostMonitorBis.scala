package co.topl.brambl.monitoring

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.kernel.Sync
import cats.implicits.catsSyntaxParallelSequence1
import cats.implicits.toTraverseOps
import co.topl.brambl.dataApi.BifrostQueryAlgebra
import co.topl.brambl.display.blockIdDisplay.display
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.monitoring.BifrostMonitorBis.AppliedBifrostBlock
import co.topl.brambl.monitoring.BifrostMonitorBis.BifrostBlockSync
import co.topl.brambl.monitoring.BifrostMonitorBis.UnappliedBifrostBlock
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.node.models.FullBlockBody
import co.topl.node.services.NodeRpcFs2Grpc
import co.topl.node.services.SynchronizationTraversalReq
import co.topl.node.services.SynchronizationTraversalRes
import co.topl.node.services.SynchronizationTraversalRes.Status.Applied
import co.topl.node.services.SynchronizationTraversalRes.Status.Empty
import co.topl.node.services.SynchronizationTraversalRes.Status.Unapplied
import fs2.Stream
import fs2.grpc.syntax.all._
import io.grpc.ManagedChannelBuilder
import io.grpc.Metadata

/**
 * Class to monitor incoming bifrost blocks via an iterator.
 * @param blockIterator The iterator in which block changes will be added to
 * @param startingBlocks Past blocks that should be reported.
 */
class BifrostMonitorBis(
  blockIterator:  Stream[IO, SynchronizationTraversalRes],
  getFullBlock:   BlockId => IO[(BlockHeader, FullBlockBody)],
  startingBlocks: Vector[BifrostBlockSync]
) {

  def pipe(in: Stream[IO, SynchronizationTraversalRes]): Stream[IO, BifrostBlockSync] = in.evalMapFilter(sync =>
    sync.status match {
      case Applied(blockId) =>
        IO.println(s"Applied TOPL block $blockId") >> getFullBlock(blockId).map(block => Some(AppliedBifrostBlock(block._2, blockId, block._1.height)))
      case Unapplied(blockId) =>
        IO.println(s"Unapplied TOPL block $blockId") >> getFullBlock(blockId).map(block => Some(UnappliedBifrostBlock(block._2, blockId, block._1.height)))
      case Empty => IO.pure(None)
    }
  )

  /**
   * Return a stream of block updates.
   * @return The infinite stream of block updatess. If startingBlocks was provided, they will be at the front of the stream.
   */
  def monitorBlocks(): Stream[IO, BifrostBlockSync] = Stream.emits(startingBlocks) ++
    blockIterator.through(pipe)

}

object BifrostMonitorBis {

  def channelResourceBis[F[_]: Sync](
    address:          String,
    port:             Int,
    secureConnection: Boolean
  ) =
    (if (secureConnection)
       ManagedChannelBuilder
         .forAddress(address, port)
         .useTransportSecurity()
     else
       ManagedChannelBuilder
         .forAddress(address, port)
         .usePlaintext()).resource[F]

  /**
   * A wrapper for a Bifrost Block Sync update.
   */
  trait BifrostBlockSync {
    // The bifrost Block being wrapped. This represents either an Applied block or an Unapplied block
    val block: FullBlockBody
    val id: BlockId
    val height: Long
    def transactions[F[_]]: Stream[F, IoTransaction] = Stream.emits(block.transactions)
  }

  // Represents a new block applied to the chain tip
  case class AppliedBifrostBlock(block: FullBlockBody, id: BlockId, height: Long) extends BifrostBlockSync
  // Represents an existing block that has been unapplied from the chain tip
  case class UnappliedBifrostBlock(block: FullBlockBody, id: BlockId, height: Long) extends BifrostBlockSync

  /**
   * Initialize and return a BifrostMonitorBis instance.
   * @param bifrostQuery The bifrost query api to retrieve updates from
   * @param startBlock The blockId of a past block. Used to retroactively report blocks. The bifrost monitor will report all blocks starting at this block.
   * @return An instance of a BifrostMonitorBis
   */
  def apply(
    address:          String,
    port:             Int,
    secureConnection: Boolean,
    bifrostQuery:     BifrostQueryAlgebra[IO],
    startBlock:       Option[BlockId] = None
  ): IO[Resource[IO, BifrostMonitorBis]] = {
    def getFullBlock(blockId: BlockId): IO[(BlockHeader, FullBlockBody)] = (for {
      block <- bifrostQuery.blockById(blockId)
    } yield block match {
      case None                      => throw new Exception(s"Unable to query block ${display(blockId)}")
      case Some((_, header, _, txs)) => (header, FullBlockBody(txs))
    }).recoverWith(e => getFullBlock(blockId))
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
      tipHeight        <- bifrostQuery.blockByDepth(0).map(_.map(_._2.height))
      startingBlockIds <- getBlockIds(startBlockHeight, tipHeight)
      startingBlocks <- startingBlockIds
        .map(bId => getFullBlock(bId).map(block => AppliedBifrostBlock(block._2, bId, block._1.height)))
        .sequence
    } yield for {
      channel <- channelResourceBis[IO](address, port, secureConnection)
      stub    <- NodeRpcFs2Grpc.stubResource[IO](channel)
      iterator <- IO(stub.synchronizationTraversal(SynchronizationTraversalReq(), new Metadata()).handleErrorWith { e =>
        e.printStackTrace()
        println("Error in BifrostMonitorBis")
        Stream.empty[IO]
      }).toResource
    } yield new BifrostMonitorBis(iterator, getFullBlock, startingBlocks)
  }

}
