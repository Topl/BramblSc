package co.topl.brambl.dataApi

import cats.data.OptionT
import cats.effect.kernel.{Resource, Sync}
import cats.free.Free
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockId
import co.topl.node.models.BlockBody
import io.grpc.ManagedChannel

/**
 * Defines a Bifrost Query API for interacting with a Bifrost node.
 */
trait BifrostQueryAlgebra[F[_]] {

  /**
   * Fetches a block by its height.
   * @param height The height of the block to fetch.
   * @return The BlockId, BlockBody, and contained transactions of the fetched block, if it exists.
   */
  def blockByHeight(
    height: Long
  ): F[Option[(BlockId, BlockBody, Seq[IoTransaction])]]

  /**
   * Fetches a block by its Id.
   *
   * @param blockId The Id of the block to fetch.
   * @return The BlockId, BlockBody, and contained transactions of the fetched block, if it exists.
   */
  def blockById(
    blockId: BlockId
  ): F[Option[(BlockId, BlockBody, Seq[IoTransaction])]]

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

}

object BifrostQueryAlgebra extends BifrostQueryInterpreter {

  sealed trait BifrostQueryADT[A]

  case class FetchBlockBody(blockId: BlockId) extends BifrostQueryADT[Option[BlockBody]]

  case class FetchTransaction(txId: TransactionId) extends BifrostQueryADT[Option[IoTransaction]]

  case class BlockByHeight(height: Long) extends BifrostQueryADT[Option[BlockId]]
  case class BroadcastTransaction(tx: IoTransaction) extends BifrostQueryADT[TransactionId]

  type BifrostQueryADTMonad[A] = Free[BifrostQueryADT, A]

  def fetchBlockBodyF(
    blockId: BlockId
  ): BifrostQueryADTMonad[Option[BlockBody]] =
    Free.liftF(FetchBlockBody(blockId))

  def fetchTransactionF(
    txId: TransactionId
  ): BifrostQueryADTMonad[Option[IoTransaction]] =
    Free.liftF(FetchTransaction(txId))

  def blockByHeightF(height: Long): BifrostQueryADTMonad[Option[BlockId]] =
    Free.liftF(BlockByHeight(height))

  def broadcastTransactionF(tx: IoTransaction): BifrostQueryADTMonad[TransactionId] =
    Free.liftF(BroadcastTransaction(tx))

  def make[F[_]: Sync](channelResource: Resource[F, ManagedChannel]): BifrostQueryAlgebra[F] =
    new BifrostQueryAlgebra[F] {

      override def blockById(
        blockId: BlockId
      ): F[Option[(BlockId, BlockBody, Seq[IoTransaction])]] = {
        import cats.implicits._
        interpretADT(
          channelResource,
          (for {
            blockBody <- OptionT(fetchBlockBodyF(blockId))
            transactions <- blockBody.transactionIds
              .map(txId => OptionT(fetchTransactionF(txId)))
              .sequence
          } yield (blockId, blockBody, transactions)).value
        )
      }

      override def fetchTransaction(
        txId: TransactionId
      ): F[Option[IoTransaction]] =
        interpretADT(channelResource, fetchTransactionF(txId))

      def blockByHeight(height: Long): F[Option[(BlockId, BlockBody, Seq[IoTransaction])]] = {
        import cats.implicits._
        interpretADT(
          channelResource,
          (for {
            blockId   <- OptionT(blockByHeightF(height))
            blockBody <- OptionT(fetchBlockBodyF(blockId))
            transactions <- blockBody.transactionIds
              .map(txId => OptionT(fetchTransactionF(txId)))
              .sequence
          } yield (blockId, blockBody, transactions)).value
        )
      }

      override def broadcastTransaction(tx: IoTransaction): F[TransactionId] =
        interpretADT(channelResource, broadcastTransactionF(tx))

    }
}
