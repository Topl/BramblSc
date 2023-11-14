package co.topl.brambl.servicekit

import cats.effect.IO
import cats.effect.kernel.Resource

import java.sql.{Connection, DriverManager}

/**
 * A resource that provides a connection to a wallet state database.
 */
trait WalletStateResource {

  /**
   * Creates a resource that provides a connection to a wallet state database.
   *
   * @param name the name of the file containing the wallet state database. It might be a path if needed.
   * @return a resource that provides a connection to a wallet state database.
   */
  def walletResource(name: String): Resource[IO, Connection] = Resource
    .make(
      IO.delay(
        DriverManager.getConnection(
          s"jdbc:sqlite:${name}"
        )
      )
    )(conn => IO.delay(conn.close()))
}

/**
 * A resource that provides a connection to a wallet state database.
 */

object WalletStateResource extends WalletStateResource
