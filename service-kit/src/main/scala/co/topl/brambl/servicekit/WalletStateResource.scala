package co.topl.brambl.servicekit

import cats.effect.IO
import cats.effect.kernel.Resource

import java.sql.{Connection, DriverManager}

/**
 * A resource that provides a connection to a wallet state database.
 */
trait WalletStateResource {

  def walletResource(name: String): Resource[IO, Connection] = Resource
    .make(
      IO.delay(
        DriverManager.getConnection(
          s"jdbc:sqlite:${name}"
        )
      )
    )(conn => IO.delay(conn.close()))
}
