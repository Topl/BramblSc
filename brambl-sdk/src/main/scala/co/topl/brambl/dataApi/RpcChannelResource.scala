package co.topl.brambl.dataApi

import cats.effect.kernel.Resource
import cats.effect.kernel.Sync
import io.grpc.ManagedChannelBuilder

/**
 * A resource that provides a connection to an GRPC server.
 */
trait RpcChannelResource {

  /**
   * Creates a resource that provides a connection to a GRPC server.
   *
   * @param address the host address of the GRPC server.
   * @param port    the port of the GRPC server.
   * @param secureConnection whether to use a secure connection.
   * @return a resource that provides a connection to an GRPC server.
   */
  def channelResource[F[_]: Sync](
    address:          String,
    port:             Int,
    secureConnection: Boolean
  ) =
    Resource
      .make {
        Sync[F].delay(
          if (secureConnection)
            ManagedChannelBuilder
              .forAddress(address, port)
              .build
          else
            ManagedChannelBuilder
              .forAddress(address, port)
              .usePlaintext()
              .build
        )
      }(channel => Sync[F].delay(channel.shutdown()))
}

/**
 * A resource that provides a connection to an GRPC server.
 */

object RpcChannelResource extends RpcChannelResource
