package co.topl.brambl.dataApi

import cats.arrow.FunctionK
import cats.data.Kleisli
import cats.effect.kernel.{Async, Resource, Sync}
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.node.services._
import io.grpc.{ManagedChannel, Metadata}

/**
 * Defines an interpreter for Bifrost Query API.
 */
trait BifrostQueryInterpreter {

  def interpretADT[A, F[_]: Sync](
    channelResource: Resource[F, ManagedChannel],
    computation:     BifrostQueryAlgebra.BifrostQueryADTMonad[A]
  ): F[A] = {
    type ChannelContextKlesli[A] =
      Kleisli[F, NodeRpcGrpc.NodeRpcBlockingStub, A]
    val kleisliComputation = computation.foldMap[ChannelContextKlesli](
      new FunctionK[BifrostQueryAlgebra.BifrostQueryADT, ChannelContextKlesli] {

        override def apply[A](
          fa: BifrostQueryAlgebra.BifrostQueryADT[A]
        ): ChannelContextKlesli[A] = {
          import cats.implicits._
          fa match {
            case BifrostQueryAlgebra.BlockByDepth(depth) =>
              Kleisli(blockingStub =>
                Sync[F]
                  .blocking(
                    blockingStub
                      .fetchBlockIdAtDepth(
                        FetchBlockIdAtDepthReq(depth)
                      )
                  )
                  .map(_.blockId.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.FetchBlockHeader(blockId) =>
              Kleisli(blockingStub =>
                Sync[F]
                  .blocking(
                    blockingStub
                      .fetchBlockHeader(
                        FetchBlockHeaderReq(blockId)
                      )
                  )
                  .map(_.header.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.FetchBlockBody(blockId) =>
              Kleisli(blockingStub =>
                Sync[F]
                  .blocking(
                    blockingStub
                      .fetchBlockBody(
                        FetchBlockBodyReq(blockId)
                      )
                  )
                  .map(_.body.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.FetchTransaction(txId) =>
              Kleisli(blockingStub =>
                Sync[F]
                  .blocking(
                    blockingStub
                      .fetchTransaction(
                        FetchTransactionReq(txId)
                      )
                  )
                  .map(_.transaction.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.BlockByHeight(height) =>
              Kleisli(blockingStub =>
                Sync[F]
                  .blocking(
                    blockingStub
                      .fetchBlockIdAtHeight(
                        FetchBlockIdAtHeightReq(height)
                      )
                  )
                  .map(_.blockId.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.BroadcastTransaction(tx) =>
              Kleisli(blockingStub =>
                Sync[F]
                  .blocking(
                    blockingStub
                      .broadcastTransaction(
                        BroadcastTransactionReq(tx)
                      )
                  )
                  .map(_ => (tx.computeId).asInstanceOf[A])
              )
          }
        }
      }
    )
    (for {
      channel <- channelResource
    } yield channel).use { channel =>
      kleisliComputation.run(NodeRpcGrpc.blockingStub(channel))
    }
  }

  def interpretADTStream[A, F[_] : Async](
                                    channelResource: Resource[F, ManagedChannel],
                                    computation: BifrostQueryAlgebra.BifrostQueryADTMonad[A]
                                  ): F[A] = {
    type ChannelContextKlesli[A] =
      Kleisli[F, NodeRpcFs2Grpc[F, Metadata], A]
    val kleisliComputation = computation.foldMap[ChannelContextKlesli](
      new FunctionK[BifrostQueryAlgebra.BifrostQueryADT, ChannelContextKlesli] {

        override def apply[A](
                               fa: BifrostQueryAlgebra.BifrostQueryADT[A]
                             ): ChannelContextKlesli[A] = {
          import cats.implicits._
          fa match {
            case BifrostQueryAlgebra.SynchronizationTraversal() =>
              Kleisli(blockingStub =>
                Sync[F]
                  .blocking(
                    blockingStub
                      .synchronizationTraversal(SynchronizationTraversalReq(), new Metadata())
                  )
                  .map(_.asInstanceOf[A])
              )
          }
        }
      }
    )
    (for {
      channel <- channelResource
      stubResource <- NodeRpcFs2Grpc.stubResource[F](channel)
    } yield stubResource).use { stubResource =>
      kleisliComputation.run(stubResource)
    }
  }

}
