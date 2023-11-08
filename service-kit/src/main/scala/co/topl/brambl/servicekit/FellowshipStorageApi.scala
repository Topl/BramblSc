package co.topl.brambl.servicekit

import cats.effect.kernel.{Resource, Sync}
import co.topl.brambl.dataApi.{FellowshipStorageAlgebra, WalletFellowship}

/**
 * Implementation of the FellowshipStorageAlgebra using a JDBC connection.
 */
object FellowshipStorageApi {

  /**
   * Creates an instance of the FellowshipStorageAlgebra using a JDBC connection.
   *
   * @param connection the JDBC connection.
   * @tparam F the effect type.
   * @return an instance of the FellowshipStorageAlgebra using a JDBC connection.
   */
  def make[F[_]: Sync](
    connection: Resource[F, java.sql.Connection]
  ): FellowshipStorageAlgebra[F] = new FellowshipStorageAlgebra[F] {

    override def addFellowship(walletEntity: WalletFellowship): F[Int] =
      connection.use { conn =>
        import cats.implicits._
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          inserted <- Sync[F].blocking(
            stmnt.executeUpdate(
              s"INSERT INTO fellowships (fellowship) VALUES ('${walletEntity.name}')"
            )
          )
        } yield inserted
      }

    override def findFellowships(): F[Seq[WalletFellowship]] =
      connection.use { conn =>
        import cats.implicits._
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs    <- Sync[F].blocking(stmnt.executeQuery("SELECT * FROM fellowships"))
        } yield LazyList
          .unfold(rs) { rs =>
            if (rs.next()) {
              Some(
                (
                  WalletFellowship(
                    rs.getInt("x_fellowship"),
                    rs.getString("fellowship")
                  ),
                  rs
                )
              )
            } else {
              None
            }
          }
          .force
          .toSeq
      }
  }
}
