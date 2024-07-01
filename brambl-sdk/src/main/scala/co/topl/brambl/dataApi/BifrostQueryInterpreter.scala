package co.topl.brambl.dataApi

import cats.arrow.FunctionK
import cats.data.Kleisli
import cats.effect.kernel.{Resource, Sync}
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.node.services._
import io.grpc.ManagedChannel

/**
 * Defines an interpreter for Bifrost Query API.
 */
trait BifrostQueryInterpreter {

  def interpretADT[A, F[_]: Sync](
    channelResource: Resource[F, ManagedChannel],
    computation:     BifrostQueryAlgebra.BifrostQueryADTMonad[A]
  ): F[A] = {
    type ChannelContextKlesli[A] =
      Kleisli[F, (NodeRpcGrpc.NodeRpcBlockingStub, RegtestRpcGrpc.RegtestRpcBlockingStub), A]
    val kleisliComputation = computation.foldMap[ChannelContextKlesli](
      new FunctionK[BifrostQueryAlgebra.BifrostQueryADT, ChannelContextKlesli] {

        override def apply[A](
          fa: BifrostQueryAlgebra.BifrostQueryADT[A]
        ): ChannelContextKlesli[A] = {
          import cats.implicits._
          fa match {
            case BifrostQueryAlgebra.MakeBlock(nbOfBlocks) =>
              Kleisli(blockingStubAndRegTestStub =>
                Sync[F]
                  .blocking(
                    blockingStubAndRegTestStub._2
                      .makeBlocks(
                        MakeBlocksReq(nbOfBlocks)
                      )
                  )
                  .map(_ => ())
                  .map(_.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.BlockByDepth(depth) =>
              Kleisli(blockingStubAndRegTestStub =>
                Sync[F]
                  .blocking(
                    blockingStubAndRegTestStub._1
                      .fetchBlockIdAtDepth(
                        FetchBlockIdAtDepthReq(depth)
                      )
                  )
                  .map(_.blockId.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.FetchBlockHeader(blockId) =>
              Kleisli(blockingStubAndRegTestStub =>
                Sync[F]
                  .blocking(
                    blockingStubAndRegTestStub._1
                      .fetchBlockHeader(
                        FetchBlockHeaderReq(blockId)
                      )
                  )
                  .map(_.header.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.FetchBlockBody(blockId) =>
              Kleisli(blockingStubAndRegTestStub =>
                Sync[F]
                  .blocking(
                    blockingStubAndRegTestStub._1
                      .fetchBlockBody(
                        FetchBlockBodyReq(blockId)
                      )
                  )
                  .map(_.body.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.FetchTransaction(txId) =>
              Kleisli(blockingStubAndRegTestStub =>
                Sync[F]
                  .blocking(
                    blockingStubAndRegTestStub._1
                      .fetchTransaction(
                        FetchTransactionReq(txId)
                      )
                  )
                  .map(_.transaction.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.BlockByHeight(height) =>
              Kleisli(blockingStubAndRegTestStub =>
                Sync[F]
                  .blocking(
                    blockingStubAndRegTestStub._1
                      .fetchBlockIdAtHeight(
                        FetchBlockIdAtHeightReq(height)
                      )
                  )
                  .map(_.blockId.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.SynchronizationTraversal() =>
              Kleisli(_ => // FIXME: check if this fixes bug
                channelResource.allocated
                  .flatMap { channel =>
                    Sync[F]
                      .blocking(
                        NodeRpcGrpc
                          .blockingStub(channel._1)
                          .synchronizationTraversal(SynchronizationTraversalReq())
                      )
                      .map(_.asInstanceOf[A])
                  }
              // Sync[F]
              //   .blocking(
              //     NodeRpcGrpc.blockingStub()
              //       .synchronizationTraversal(SynchronizationTraversalReq())
              //   )
              //   .map(_.asInstanceOf[A])
              )
            case BifrostQueryAlgebra.BroadcastTransaction(tx) =>
              Kleisli(blockingStubAndRegTestStub =>
                Sync[F]
                  .blocking(
                    blockingStubAndRegTestStub._1
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
      kleisliComputation.run((NodeRpcGrpc.blockingStub(channel), RegtestRpcGrpc.blockingStub(channel)))
    }
  }

}
