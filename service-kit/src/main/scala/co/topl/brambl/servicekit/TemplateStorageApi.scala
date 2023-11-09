package co.topl.brambl.servicekit

import cats.effect.kernel.{Resource, Sync}
import co.topl.brambl.dataApi.{TemplateStorageAlgebra, WalletTemplate}

/**
 * Implementation of the TemplateStorageAlgebra using a JDBC connection.
 */
object TemplateStorageApi {

  /**
   * Creates an instance of the TemplateStorageAlgebra using a JDBC connection.
   *
   * @param connection the JDBC connection.
   * @return an instance of the TemplateStorageAlgebra using a JDBC connection.
   */
  def make[F[_]: Sync](
    connection: Resource[F, java.sql.Connection]
  ): TemplateStorageAlgebra[F] = new TemplateStorageAlgebra[F] {

    override def addTemplate(walletTemplate: WalletTemplate): F[Int] =
      connection.use { conn =>
        import cats.implicits._
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          inserted <- Sync[F].blocking(
            stmnt.executeUpdate(
              s"INSERT INTO templates (template, lock) VALUES ('${walletTemplate.name}', '${walletTemplate.lockTemplate}')"
            )
          )
        } yield inserted
      }

    override def findTemplates(): F[Seq[WalletTemplate]] =
      connection.use { conn =>
        import cats.implicits._
        import io.circe.parser._
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs    <- Sync[F].blocking(stmnt.executeQuery("SELECT * FROM templates"))
        } yield LazyList
          .unfold(rs) { rs =>
            if (rs.next()) {
              Some(
                (
                  WalletTemplate(
                    rs.getInt("y_template"),
                    rs.getString("template"),
                    parse(rs.getString("lock")).toOption.get.noSpaces
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
