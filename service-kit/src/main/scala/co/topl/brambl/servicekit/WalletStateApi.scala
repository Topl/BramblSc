package co.topl.brambl.servicekit

import cats.data.{OptionT, Validated, ValidatedNel}
import cats.effect.kernel.{Resource, Sync}
import cats.implicits._
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.builders.locks.{LockTemplate, PropositionTemplate}
import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.codecs.LockTemplateCodecs.{decodeLockTemplate, encodeLockTemplate}
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.dataApi.WalletStateAlgebra
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.{Indices, LockAddress, LockId}
import co.topl.brambl.utils.Encoding
import co.topl.brambl.wallet.WalletApi
import com.google.protobuf.ByteString
import io.circe.parser._
import io.circe.syntax.EncoderOps
import quivr.models.{KeyPair, Preimage, Proposition, VerificationKey}

import java.sql.Statement
import scala.collection.mutable.ListBuffer

/**
 * An implementation of the WalletInteractionAlgebra that uses a database to store state information.
 */
object WalletStateApi {

  /**
   * Creates an instance of the WalletStateAlgebra that uses a database to store state information.
   *
   * @param connection the JDBC connection.
   * @param walletApi the wallet api.
   * @return an instance of the WalletStateAlgebra that uses a database to store state information.
   */
  def make[F[_]: Sync](
    connection: Resource[F, java.sql.Connection],
    walletApi:  WalletApi[F]
  ): WalletStateAlgebra[F] =
    new WalletStateAlgebra[F] {

      private def getFellowshipIdx(fellowship: String, stmnt: Statement) = OptionT(for {
        rs <- Sync[F].blocking(
          stmnt.executeQuery(
            s"SELECT x_fellowship, fellowship FROM fellowships WHERE fellowship = '${fellowship}'"
          )
        )
        x <- Sync[F].delay(rs.getInt("x_fellowship"))
      } yield if (rs.next()) Some(x) else None)

      private def getTemplateIdx(template: String, stmnt: Statement) = OptionT(for {
        rs <- Sync[F].blocking(
          stmnt.executeQuery(
            s"SELECT y_template, template FROM templates WHERE template = '${template}'"
          )
        )
        y <- Sync[F].delay(rs.getInt("y_template"))
      } yield if (rs.next()) Some(y) else None)

      private def getList(x: Int, y: Int, stmnt: Statement) = OptionT.liftF(for {
        rs <- Sync[F].blocking(
          stmnt.executeQuery(
            "SELECT x_fellowship, y_template, z_interaction, address FROM cartesian " +
            s"WHERE x_fellowship = ${x} AND y_template = ${y}"
          )
        )
        hasNext <- Sync[F].delay(rs.next())
        pair <- Sync[F].iterateWhileM((hasNext, List[(Indices, String)]())) { x =>
          val (_, list) = x
          for {
            x       <- Sync[F].delay(rs.getInt("x_fellowship"))
            y       <- Sync[F].delay(rs.getInt("y_template"))
            z       <- Sync[F].delay(rs.getInt("z_interaction"))
            address <- Sync[F].delay(rs.getString("address"))
            hasNext <- Sync[F].delay(rs.next())
          } yield (hasNext, (Indices(x, y, z), address) :: list)
        }(_._1)
      } yield pair._2)

      override def getInteractionList(fellowship: String, template: String): F[Option[List[(Indices, String)]]] =
        connection.use { conn =>
          (for {
            stmnt <- OptionT.liftF(Sync[F].blocking(conn.createStatement()))
            x     <- getFellowshipIdx(fellowship, stmnt)
            y     <- getTemplateIdx(template, stmnt)
            list  <- getList(x, y, stmnt)
          } yield list).value
        }

      override def getIndicesBySignature(signatureProposition: Proposition.DigitalSignature): F[Option[Indices]] =
        connection.use { conn =>
          for {
            stmnt <- Sync[F].blocking(conn.createStatement())
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT x_fellowship, y_template, z_interaction, routine, vk FROM " +
                s"cartesian WHERE routine = '${signatureProposition.routine}' AND " +
                s"vk = '${Encoding.encodeToBase58(signatureProposition.verificationKey.toByteArray)}'"
              )
            )
            hasNext <- Sync[F].delay(rs.next())
            x       <- Sync[F].delay(rs.getInt("x_fellowship"))
            y       <- Sync[F].delay(rs.getInt("y_template"))
            z       <- Sync[F].delay(rs.getInt("z_interaction"))
          } yield if (hasNext) Some(Indices(x, y, z)) else None
        }

      override def getIndicesByAddress(lockAddress: String): F[Option[Indices]] =
        connection.use { conn =>
          for {
            stmnt <- Sync[F].blocking(conn.createStatement())
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT x_fellowship, y_template, z_interaction FROM " +
                s"cartesian WHERE address = '$lockAddress'"
              )
            )
            hasNext <- Sync[F].delay(rs.next())
            x       <- Sync[F].delay(rs.getInt("x_fellowship"))
            y       <- Sync[F].delay(rs.getInt("y_template"))
            z       <- Sync[F].delay(rs.getInt("z_interaction"))
          } yield if (hasNext) Some(Indices(x, y, z)) else None
        }

      def getLockByIndex(indices: Indices): F[Option[Lock.Predicate]] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT x_fellowship, y_template, z_interaction, lock_predicate FROM " +
              s"cartesian WHERE x_fellowship = ${indices.x} AND " +
              s"y_template = ${indices.y} AND " +
              s"z_interaction = ${indices.z}"
            )
          )
          _                 <- Sync[F].delay(rs.next())
          someLockPredicate <- Sync[F].delay(rs.getString("lock_predicate"))
        } yield Option(someLockPredicate).map(lock_predicate =>
          Lock.Predicate.parseFrom(
            Encoding.decodeFromBase58Check(lock_predicate).toOption.get
          )
        )
      }

      def setCurrentIndices(fellowship: String, template: String, interaction: Int): F[Option[Indices]] =
        connection.use { conn =>
          for {
            stmnt <- Sync[F].blocking(conn.createStatement())
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT x_fellowship, fellowship FROM fellowships WHERE fellowship = '${fellowship}'"
              )
            )
            x <- Sync[F].delay(rs.getInt("x_fellowship"))
            query =
              s"SELECT y_template, template FROM templates WHERE template = '${template}'"
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                query
              )
            )
            y <- Sync[F].delay(rs.getInt("y_template"))
            res <- Sync[F].blocking(
              stmnt.executeUpdate(
                s"DELETE FROM " +
                s"cartesian WHERE x_fellowship = ${x} AND " +
                s"y_template = ${y} AND " +
                s"z_interaction > ${interaction}"
              )
            )
          } yield if (res > 0) Some(Indices(x, y, interaction)) else None
        }

      def getLockByAddress(lockAddress: String): F[Option[Lock.Predicate]] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT address, lock_predicate FROM " +
              s"cartesian WHERE address = '$lockAddress'"
            )
          )
          hasNext        <- Sync[F].delay(rs.next())
          lock_predicate <- Sync[F].delay(rs.getString("lock_predicate"))
        } yield
          if (hasNext)
            Some(
              Lock.Predicate.parseFrom(
                Encoding.decodeFromBase58Check(lock_predicate).toOption.get
              )
            )
          else None
      }

      override def updateWalletState(
        lockPredicate: String,
        lockAddress:   String,
        routine:       Option[String],
        vk:            Option[String],
        indices:       Indices
      ): F[Unit] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          statement =
            s"INSERT INTO cartesian (x_fellowship, y_template, z_interaction, lock_predicate, address, routine, vk) VALUES (${indices.x}, ${indices.y}, ${indices.z}, '${lockPredicate}', '" +
              lockAddress + "', " + routine
                .map(x => s"'$x'")
                .getOrElse("NULL") + ", " + vk
                .map(x => s"'$x'")
                .getOrElse("NULL") + ")"
          _ <- Sync[F].blocking(
            stmnt.executeUpdate(statement)
          )
        } yield ()
      }

      override def getNextIndicesForFunds(fellowship: String, template: String): F[Option[Indices]] = connection.use {
        conn =>
          for {
            stmnt <- Sync[F].blocking(conn.createStatement())
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT x_fellowship, fellowship FROM fellowships WHERE fellowship = '${fellowship}'"
              )
            )
            x <- Sync[F].delay(rs.getInt("x_fellowship"))
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT y_template, template FROM templates WHERE template = '${template}'"
              )
            )
            y <- Sync[F].delay(rs.getInt("y_template"))
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT x_fellowship, y_template, MAX(z_interaction) as z_index FROM cartesian WHERE x_fellowship = ${x} AND y_template = ${y}"
              )
            )
            z <- Sync[F].delay(rs.getInt("z_index"))
          } yield if (rs.next()) Some(Indices(x, y, z + 1)) else None
      }

      private def validateFellowship(
        fellowship: String
      ): F[ValidatedNel[String, String]] =
        connection.use { conn =>
          for {
            stmnt <- Sync[F].blocking(conn.createStatement())
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT x_fellowship, fellowship FROM fellowships WHERE fellowship = '${fellowship}'"
              )
            )
          } yield
            if (rs.next()) Validated.validNel(fellowship)
            else Validated.invalidNel("Fellowship not found")
        }

      private def validateTemplate(
        template: String
      ): F[ValidatedNel[String, String]] =
        connection.use { conn =>
          for {
            stmnt <- Sync[F].blocking(conn.createStatement())
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT y_template, template FROM templates WHERE template = '${template}'"
              )
            )
          } yield
            if (rs.next()) Validated.validNel(template)
            else Validated.invalidNel("Template not found")
        }

      def validateCurrentIndicesForFunds(
        fellowship:      String,
        template:        String,
        someInteraction: Option[Int]
      ): F[ValidatedNel[String, Indices]] = for {
        validatedFellowship <- validateFellowship(fellowship)
        validatedTemplate   <- validateTemplate(template)
        indices             <- getCurrentIndicesForFunds(fellowship, template, someInteraction)
      } yield (
        validatedFellowship,
        validatedTemplate,
        indices.toValidNel("Indices not found")
      ).mapN((_, _, index) => index)

      override def getAddress(
        fellowship:      String,
        template:        String,
        someInteraction: Option[Int]
      ): F[Option[String]] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT x_fellowship, fellowship FROM fellowships WHERE fellowship = '${fellowship}'"
            )
          )
          x <- Sync[F].delay(rs.getInt("x_fellowship"))
          query =
            s"SELECT y_template, template FROM templates WHERE template = '${template}'"
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              query
            )
          )
          y <- Sync[F].delay(rs.getInt("y_template"))
          query = s"SELECT address, x_fellowship, y_template, " + someInteraction
            .map(_ => "z_interaction as z_index")
            .getOrElse(
              "MAX(z_interaction) as z_index"
            ) + s" FROM cartesian WHERE x_fellowship = ${x} AND y_template = ${y}" + someInteraction
            .map(x => s" AND z_interaction = ${x}")
            .getOrElse("")
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              query
            )
          )
          address <- Sync[F].delay(Option(rs.getString("address")))
        } yield address
      }

      override def getCurrentIndicesForFunds(
        fellowship:      String,
        template:        String,
        someInteraction: Option[Int]
      ): F[Option[Indices]] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT x_fellowship, fellowship FROM fellowships WHERE fellowship = '${fellowship}'"
            )
          )
          x <- Sync[F].delay(rs.getInt("x_fellowship"))
          query =
            s"SELECT y_template, template FROM templates WHERE template = '${template}'"
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              query
            )
          )
          y <- Sync[F].delay(rs.getInt("y_template"))
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT x_fellowship, y_template, " + someInteraction
                .map(_ => "z_interaction as z_index")
                .getOrElse(
                  "MAX(z_interaction) as z_index"
                ) + s" FROM cartesian WHERE x_fellowship = ${x} AND y_template = ${y}" + someInteraction
                .map(x => s" AND z_interaction = ${x}")
                .getOrElse("")
            )
          )
          z <- someInteraction
            .map(x => Sync[F].point(x))
            .getOrElse(Sync[F].delay(rs.getInt("z_index")))
        } yield if (rs.next()) Some(Indices(x, y, z)) else None
      }

      override def getCurrentAddress: F[String] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              "SELECT address, MAX(z_interaction) FROM cartesian WHERE x_fellowship = 1 AND y_template = 1  group by x_fellowship, y_template"
            )
          )
          lockAddress <- Sync[F].delay(rs.getString("address"))
        } yield lockAddress
      }

      override def initWalletState(
        networkId: Int,
        ledgerId:  Int,
        mainKey:   KeyPair
      ): F[Unit] = {
        import TransactionBuilderApi.implicits._
        import cats.implicits._
        import co.topl.brambl.common.ContainsEvidence.Ops
        import co.topl.brambl.common.ContainsImmutable.instances._
        connection.use { conn =>
          for {
            stmnt <- Sync[F].delay(conn.createStatement())
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE TABLE IF NOT EXISTS cartesian (id INTEGER PRIMARY KEY," +
                " x_fellowship INTEGER NOT NULL, y_template INTEGER NOT NULL, z_interaction INTEGER NOT NULL, " +
                "lock_predicate TEXT NOT NULL, address TEXT NOT NULL, routine TEXT, vk TEXT)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE TABLE IF NOT EXISTS fellowships (fellowship TEXT," +
                " x_fellowship INTEGER PRIMARY KEY ASC)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE TABLE IF NOT EXISTS templates (template TEXT NOT NULL," +
                " y_template INTEGER PRIMARY KEY ASC,  lock TEXT NOT NULL)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE TABLE IF NOT EXISTS verification_keys (x_fellowship INTEGER NOT NULL," +
                " y_template INTEGER NOT NULL, vks TEXT NOT NULL, PRIMARY KEY (x_fellowship, y_template))"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE TABLE IF NOT EXISTS digests (id INTEGER PRIMARY KEY, digest_evidence TEXT NOT NULL," +
                " preimage_input TEXT NOT NULL, preimage_salt TEXT NOT NULL)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE UNIQUE INDEX IF NOT EXISTS digest_idx ON digests (digest_evidence)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE UNIQUE INDEX IF NOT EXISTS template_names_idx ON templates (template)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE INDEX IF NOT EXISTS fellowship_names_idx ON fellowships (fellowship)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE UNIQUE INDEX IF NOT EXISTS cartesian_coordinates ON cartesian (x_fellowship, y_template, z_interaction)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE INDEX IF NOT EXISTS signature_idx ON cartesian (routine, vk)"
              )
            )
            defaultTemplate <- Sync[F].delay(
              LockTemplate.PredicateTemplate[F](
                List(
                  PropositionTemplate.SignatureTemplate[F]("ExtendedEd25519", 0)
                ),
                1
              )
            )
            genesisTemplate <- Sync[F].delay(
              LockTemplate.PredicateTemplate[F](
                List(
                  PropositionTemplate
                    .HeightTemplate[F]("header", 1, Long.MaxValue)
                ),
                1
              )
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                "INSERT INTO fellowships (fellowship, x_fellowship) VALUES ('nofellowship', 0)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                "INSERT INTO fellowships (fellowship, x_fellowship) VALUES ('self', 1)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                s"INSERT INTO templates (template, y_template, lock) VALUES ('default', 1, '${encodeLockTemplate(defaultTemplate).toString}')"
              )
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                s"INSERT INTO templates (template, y_template, lock) VALUES ('genesis', 2, '${encodeLockTemplate(genesisTemplate).toString}')"
              )
            )
            selfDefaultVk <- walletApi.deriveChildKeysPartial(mainKey, 1, 1).map(_.vk)
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                s"INSERT INTO verification_keys (x_fellowship, y_template, vks) VALUES (1, 1, '${List(Encoding.encodeToBase58(selfDefaultVk.toByteArray)).asJson.toString}')"
              )
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                s"INSERT INTO verification_keys (x_fellowship, y_template, vks) VALUES (0, 2, '${List[String]().asJson.toString}')"
              )
            )
            defaultSignatureLock <- getLock("self", "default", 1).map(_.get)
            signatureLockAddress = LockAddress(
              networkId,
              ledgerId,
              LockId(Lock().withPredicate(defaultSignatureLock.getPredicate).sizedEvidence.digest.value)
            )
            childVk           <- walletApi.deriveChildVerificationKey(selfDefaultVk, 1)
            genesisHeightLock <- getLock("nofellowship", "genesis", 1).map(_.get)
            heightLockAddress = LockAddress(
              networkId,
              ledgerId,
              LockId(Lock().withPredicate(genesisHeightLock.getPredicate).sizedEvidence.digest.value)
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                "INSERT INTO cartesian (x_fellowship, y_template, z_interaction, lock_predicate, address, routine, vk) VALUES (1, 1, 1, '" +
                Encoding
                  .encodeToBase58Check(
                    defaultSignatureLock.getPredicate.toByteArray
                  ) +
                "', '" +
                signatureLockAddress.toBase58() + "', " + "'ExtendedEd25519', " + "'" +
                Encoding.encodeToBase58(childVk.toByteArray)
                + "'" + ")"
              )
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                "INSERT INTO cartesian (x_fellowship, y_template, z_interaction, lock_predicate, address) VALUES (0, 2, 1, '" +
                Encoding
                  .encodeToBase58Check(
                    genesisHeightLock.getPredicate.toByteArray
                  ) +
                "', '" +
                heightLockAddress.toBase58() + "')"
              )
            )
            _ <- Sync[F].delay(stmnt.close())
          } yield ()
        }
      }

      override def getPreimage(
        digestProposition: Proposition.Digest
      ): F[Option[Preimage]] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT preimage_input, preimage_salt FROM digests WHERE " +
              s"digest_evidence = '${Encoding.encodeToBase58Check(digestProposition.sizedEvidence.digest.value.toByteArray)}'"
            )
          )
          hasNext       <- Sync[F].delay(rs.next())
          preimageInput <- Sync[F].delay(rs.getString("preimage_input"))
          preimageSalt  <- Sync[F].delay(rs.getString("preimage_salt"))
        } yield
          if (hasNext) {
            val input = Encoding.decodeFromBase58Check(preimageInput).toOption.get
            val salt = Encoding.decodeFromBase58Check(preimageSalt).toOption.get
            Preimage(ByteString.copyFrom(input), ByteString.copyFrom(salt)).some
          } else None
      }

      override def addPreimage(preimage: Preimage, digestProposition: Proposition.Digest): F[Unit] =
        connection.use { conn =>
          for {
            stmnt <- Sync[F].blocking(conn.createStatement())
            statement = {
              val digestEvidence =
                Encoding.encodeToBase58Check(digestProposition.sizedEvidence.digest.value.toByteArray)
              val input = Encoding.encodeToBase58Check(preimage.input.toByteArray)
              val salt = Encoding.encodeToBase58Check(preimage.salt.toByteArray)
              s"INSERT INTO digests (digest_evidence, preimage_input, preimage_salt) VALUES " +
              s"('${digestEvidence}', '${input}', '${salt}')"
            }
            _ <- Sync[F].blocking(
              stmnt.executeUpdate(statement)
            )
          } yield ()
        }

      override def addEntityVks(
        fellowship: String,
        template:   String,
        fellows:    List[String]
      ): F[Unit] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT x_fellowship FROM fellowships WHERE fellowship = '${fellowship}'"
            )
          )
          x <- Sync[F].delay(rs.getInt("x_fellowship"))
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT y_template FROM templates WHERE template = '${template}'"
            )
          )
          y <- Sync[F].delay(rs.getInt("y_template"))
          statement =
            s"INSERT INTO verification_keys (x_fellowship, y_template, vks) VALUES (${x}, ${y}, " +
              s"'${fellows.asJson.toString}')"
          _ <- Sync[F].blocking(
            stmnt.executeUpdate(statement)
          )
        } yield ()
      }

      override def getEntityVks(
        fellowship: String,
        template:   String
      ): F[Option[List[String]]] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT x_fellowship FROM fellowships WHERE fellowship = '${fellowship}'"
            )
          )
          x <- Sync[F].delay(rs.getInt("x_fellowship"))
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT y_template FROM templates WHERE template = '${template}'"
            )
          )
          y <- Sync[F].delay(rs.getInt("y_template"))
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT vks FROM verification_keys WHERE x_fellowship = ${x} AND y_template = ${y}"
            )
          )
          vks <- Sync[F].delay(rs.getString("vks"))
        } yield
          if (!rs.next()) None
          else parse(vks).toOption.flatMap(_.as[List[String]].toOption)
      }

      override def addNewLockTemplate(
        template:     String,
        lockTemplate: LockTemplate[F]
      ): F[Unit] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          // FIXME: do not use max, use autoincrement
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT MAX(y_template) as y_index FROM templates"
            )
          )
          y <- Sync[F].delay(rs.getInt("y_index"))
          statement =
            s"INSERT INTO templates (template, y_template, lock) VALUES ('${template}', ${y + 1}, '${encodeLockTemplate(lockTemplate).toString}')"
          _ <- Sync[F].blocking(
            stmnt.executeUpdate(statement)
          )
        } yield ()
      }

      override def getLockTemplate(
        template: String
      ): F[Option[LockTemplate[F]]] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT lock FROM templates WHERE template = '${template}'"
            )
          )
          lockStr <- Sync[F].delay(rs.getString("lock"))
        } yield
          if (!rs.next()) None
          else
            parse(lockStr).toOption.flatMap(decodeLockTemplate[F](_).toOption)
      }

      override def getLock(
        fellowship:      String,
        template:        String,
        nextInteraction: Int
      ): F[Option[Lock]] = for {
        changeTemplate <- getLockTemplate(template)
        entityVks <- getEntityVks(fellowship, template)
          .map(
            _.map(
              _.map(vk =>
                VerificationKey.parseFrom(
                  Encoding.decodeFromBase58(vk).toOption.get
                )
              )
            )
          )
        childVks <- entityVks
          .map(vks => vks.map(walletApi.deriveChildVerificationKey(_, nextInteraction)).sequence)
          .sequence
        changeLock <- changeTemplate
          .flatMap(template => childVks.map(vks => template.build(vks).map(_.toOption)))
          .sequence
          .map(_.flatten)
      } yield changeLock

      override def getCurrentAddresses(): F[Seq[LockAddress]] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT MAX(z_interaction), address FROM cartesian GROUP BY x_fellowship, y_template"
            )
          )
        } yield {
          val addresses: ListBuffer[LockAddress] = ListBuffer.empty[LockAddress]
          while (rs.next()) {
            val address = rs.getString("address")
            addresses += AddressCodecs.decodeAddress(address).toOption.get
          }
          addresses.toSeq
        }
      }
    }
}
