package co.topl.brambl.dataApi

import cats.data.OptionT
import cats.effect.kernel.{Resource, Sync}
import cats.free.Free
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.node.models.BlockBody
import co.topl.node.services.SynchronizationTraversalRes
import io.grpc.ManagedChannel
import io.grpc.stub.StreamObserver

/**
 * Defines a Bifrost Query API for interacting with a Bifrost node.
 */
trait BifrostQueryAlgebra[F[_]] {

  /**
   * Fetches a block by its height.
   * @param height The height of the block to fetch.
   * @return The BlockId, BlockHeader, BlockBody, and contained transactions of the fetched block, if it exists.
   */
  def blockByHeight(
    height: Long
  ): F[Option[(BlockId, BlockHeader, BlockBody, Seq[IoTransaction])]]

  /**
   * Fetches a block by its depth.
   * @param height The depth of the block to fetch. The depth 1 is the tip of the chain.
   * @return The BlockId, BlockHeader, BlockBody, and contained transactions of the fetched block, if it exists.
   */
  def blockByDepth(
    depth: Long
  ): F[Option[(BlockId, BlockHeader, BlockBody, Seq[IoTransaction])]]

  /**
   * Fetches a block by its Id.
   *
   * @param blockId The Id of the block to fetch.
   * @return The BlockId, BlockHeader, BlockBody, and contained transactions of the fetched block, if it exists.
   */
  def blockById(
    blockId: BlockId
  ): F[Option[(BlockId, BlockHeader, BlockBody, Seq[IoTransaction])]]

  /**
   * Fetches a transaction by its Id.
   *
   * @param txId The Id of the transaction to fetch.
   * @return The fetched transaction, if it exists.
   */
  def fetchTransaction(txId: TransactionId): F[Option[IoTransaction]]

  /**
   * Broadcasts a transaction to the network.
   *
   * @param tx The transaction to broadcast.
   * @return The Id of the transaction that was broadcasted.
   */
  def broadcastTransaction(tx: IoTransaction): F[TransactionId]

  /**
   * Observe the stream of changes to the canonical head of the chain.
   * @param observer the observer of the stream
   */
  def synchronizationTraversal(observer: StreamObserver[SynchronizationTraversalRes]): F[Unit]

  /**
   * Make a block in regtest mode.
   *
   * @param nbOfBlocks the number of blocks to mint.
   */
  def makeBlock(
    nbOfBlocks: Int
  ): F[Unit]

}

object BifrostQueryAlgebra extends BifrostQueryInterpreter {

  sealed trait BifrostQueryADT[A]

  case class FetchBlockBody(blockId: BlockId) extends BifrostQueryADT[Option[BlockBody]]

  case class FetchBlockHeader(blockId: BlockId) extends BifrostQueryADT[Option[BlockHeader]]

  case class FetchTransaction(txId: TransactionId) extends BifrostQueryADT[Option[IoTransaction]]

  case class BlockByHeight(height: Long) extends BifrostQueryADT[Option[BlockId]]

  case class BlockByDepth(depth: Long) extends BifrostQueryADT[Option[BlockId]]

  case class MakeBlock(nbOfBlocks: Int) extends BifrostQueryADT[Unit]

  case class SynchronizationTraversal(observer: StreamObserver[SynchronizationTraversalRes])
      extends BifrostQueryADT[Unit]

  case class BroadcastTransaction(tx: IoTransaction) extends BifrostQueryADT[TransactionId]

  type BifrostQueryADTMonad[A] = Free[BifrostQueryADT, A]

  def makeBlockF(
    nbOfBlocks: Int
  ): BifrostQueryADTMonad[Unit] =
    Free.liftF(MakeBlock(nbOfBlocks))

  def fetchBlockBodyF(
    blockId: BlockId
  ): BifrostQueryADTMonad[Option[BlockBody]] =
    Free.liftF(FetchBlockBody(blockId))

  def fetchBlockHeaderF(
    blockId: BlockId
  ): BifrostQueryADTMonad[Option[BlockHeader]] =
    Free.liftF(FetchBlockHeader(blockId))

  def fetchTransactionF(
    txId: TransactionId
  ): BifrostQueryADTMonad[Option[IoTransaction]] =
    Free.liftF(FetchTransaction(txId))

  def blockByHeightF(height: Long): BifrostQueryADTMonad[Option[BlockId]] =
    Free.liftF(BlockByHeight(height))

  def blockByDepthF(depth: Long): BifrostQueryADTMonad[Option[BlockId]] =
    Free.liftF(BlockByDepth(depth))

  def synchronizationTraversalF(observer: StreamObserver[SynchronizationTraversalRes]): BifrostQueryADTMonad[Unit] =
    Free.liftF(SynchronizationTraversal(observer))

  def broadcastTransactionF(tx: IoTransaction): BifrostQueryADTMonad[TransactionId] =
    Free.liftF(BroadcastTransaction(tx))

  def make[F[_]: Sync](channelResource: Resource[F, ManagedChannel]): BifrostQueryAlgebra[F] =
    new BifrostQueryAlgebra[F] {

      override def makeBlock(nbOfBlocks: Int): F[Unit] = interpretADT(channelResource, makeBlockF(nbOfBlocks))

      override def blockByDepth(depth: Long): F[Option[(BlockId, BlockHeader, BlockBody, Seq[IoTransaction])]] = {
        import cats.implicits._
        interpretADT(
          channelResource,
          (for {
            blockId     <- OptionT(blockByDepthF(depth))
            blockHeader <- OptionT(fetchBlockHeaderF(blockId))
            blockBody   <- OptionT(fetchBlockBodyF(blockId))
            transactions <- blockBody.transactionIds
              .map(txId => OptionT(fetchTransactionF(txId)))
              .sequence
          } yield (blockId, blockHeader, blockBody, transactions)).value
        )
      }

      override def blockById(
        blockId: BlockId
      ): F[Option[(BlockId, BlockHeader, BlockBody, Seq[IoTransaction])]] = {
        import cats.implicits._
        interpretADT(
          channelResource,
          (for {
            blockBody   <- OptionT(fetchBlockBodyF(blockId))
            blockHeader <- OptionT(fetchBlockHeaderF(blockId))
            transactions <- blockBody.transactionIds
              .map(txId => OptionT(fetchTransactionF(txId)))
              .sequence
          } yield (blockId, blockHeader, blockBody, transactions)).value
        )
      }

      override def fetchTransaction(
        txId: TransactionId
      ): F[Option[IoTransaction]] =
        interpretADT(channelResource, fetchTransactionF(txId))

      def blockByHeight(height: Long): F[Option[(BlockId, BlockHeader, BlockBody, Seq[IoTransaction])]] = {
        import cats.implicits._
        interpretADT(
          channelResource,
          (for {
            blockId     <- OptionT(blockByHeightF(height))
            blockHeader <- OptionT(fetchBlockHeaderF(blockId))
            blockBody   <- OptionT(fetchBlockBodyF(blockId))
            transactions <- blockBody.transactionIds
              .map(txId => OptionT(fetchTransactionF(txId)))
              .sequence
          } yield (blockId, blockHeader, blockBody, transactions)).value
        )
      }

      override def broadcastTransaction(tx: IoTransaction): F[TransactionId] =
        interpretADT(channelResource, broadcastTransactionF(tx))

      override def synchronizationTraversal(observer: StreamObserver[SynchronizationTraversalRes]): F[Unit] =
        interpretADT(channelResource, synchronizationTraversalF(observer))

    }
}
