package co.topl.brambl.servicekit

import cats.data.Validated
import cats.data.ValidatedNel
import cats.effect.kernel.Resource
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.builders.locks.LockTemplate
import co.topl.brambl.builders.locks.PropositionTemplate
import co.topl.brambl.codecs.LockTemplateCodecs.decodeLockTemplate
import co.topl.brambl.codecs.LockTemplateCodecs.encodeLockTemplate
import co.topl.brambl.dataApi.WalletStateAlgebra
import co.topl.brambl.models.Indices
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.LockId
import co.topl.brambl.models.box.Lock
import co.topl.brambl.utils.Encoding
import co.topl.brambl.wallet.WalletApi
import io.circe.parser._
import io.circe.syntax.EncoderOps
import quivr.models.Preimage
import quivr.models.Proposition
import quivr.models.VerificationKey

/**
 * An implementation of the WalletInteractionAlgebra that uses a database to store state information.
 */
object WalletStateApi {

  def make[F[_]: Sync](
    connection: Resource[F, java.sql.Connection],
    walletApi:  WalletApi[F]
  ): WalletStateAlgebra[F] =
    new WalletStateAlgebra[F] {

      override def getIndicesBySignature(signatureProposition: Proposition.DigitalSignature): F[Option[Indices]] =
        connection.use { conn =>
          for {
            stmnt <- Sync[F].blocking(conn.createStatement())
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT x_fellowship, y_contract, z_state, routine, vk FROM " +
                s"cartesian WHERE routine = '${signatureProposition.routine}' AND " +
                s"vk = '${Encoding.encodeToBase58(signatureProposition.verificationKey.toByteArray)}'"
              )
            )
            hasNext <- Sync[F].delay(rs.next())
            x       <- Sync[F].delay(rs.getInt("x_fellowship"))
            y       <- Sync[F].delay(rs.getInt("y_contract"))
            z       <- Sync[F].delay(rs.getInt("z_state"))
          } yield if (hasNext) Some(Indices(x, y, z)) else None
        }

      def getLockByIndex(indices: Indices): F[Option[Lock.Predicate]] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT x_fellowship, y_contract, z_state, lock_predicate FROM " +
              s"cartesian WHERE x_fellowship = ${indices.x} AND " +
              s"y_contract = ${indices.y} AND " +
              s"z_state = ${indices.z}"
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

      def setCurrentIndices(fellowship: String, template: String, interaction: Int): F[Unit] = ???

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
            s"INSERT INTO cartesian (x_fellowship, y_contract, z_state, lock_predicate, address, routine, vk) VALUES (${indices.x}, ${indices.y}, ${indices.z}, '${lockPredicate}', '" +
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

      override def getNextIndicesForFunds(fellowship: String, contract: String): F[Option[Indices]] = connection.use {
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
                s"SELECT y_contract, contract FROM templates WHERE contract = '${contract}'"
              )
            )
            y <- Sync[F].delay(rs.getInt("y_contract"))
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT x_fellowship, y_contract, MAX(z_state) as z_index FROM cartesian WHERE x_fellowship = ${x} AND y_contract = ${y}"
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
        contract: String
      ): F[ValidatedNel[String, String]] =
        connection.use { conn =>
          for {
            stmnt <- Sync[F].blocking(conn.createStatement())
            rs <- Sync[F].blocking(
              stmnt.executeQuery(
                s"SELECT y_contract, contract FROM templates WHERE contract = '${contract}'"
              )
            )
          } yield
            if (rs.next()) Validated.validNel(contract)
            else Validated.invalidNel("Template not found")
        }

      def validateCurrentIndicesForFunds(
        fellowship:           String,
        contract:        String,
        someInteraction: Option[Int]
      ): F[ValidatedNel[String, Indices]] = for {
        validatedFellowship    <- validateFellowship(fellowship)
        validatedTemplate <- validateTemplate(contract)
        indices           <- getCurrentIndicesForFunds(fellowship, contract, someInteraction)
      } yield (
        validatedFellowship,
        validatedTemplate,
        indices.toValidNel("Indices not found")
      ).mapN((_, _, index) => index)

      override def getAddress(
        fellowship:           String,
        contract:        String,
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
            s"SELECT y_contract, contract FROM templates WHERE contract = '${contract}'"
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              query
            )
          )
          y <- Sync[F].delay(rs.getInt("y_contract"))
          query = s"SELECT address, x_fellowship, y_contract, " + someInteraction
            .map(_ => "z_state as z_index")
            .getOrElse(
              "MAX(z_state) as z_index"
            ) + s" FROM cartesian WHERE x_fellowship = ${x} AND y_contract = ${y}" + someInteraction
            .map(x => s" AND z_state = ${x}")
            .getOrElse("")
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              query
            )
          )
          address <- Sync[F].delay(rs.getString("address"))
        } yield if (rs.next()) Some(address) else None
      }

      override def getCurrentIndicesForFunds(
        fellowship:           String,
        contract:        String,
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
            s"SELECT y_contract, contract FROM templates WHERE contract = '${contract}'"
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              query
            )
          )
          y <- Sync[F].delay(rs.getInt("y_contract"))
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT x_fellowship, y_contract, " + someInteraction
                .map(_ => "z_state as z_index")
                .getOrElse(
                  "MAX(z_state) as z_index"
                ) + s" FROM cartesian WHERE x_fellowship = ${x} AND y_contract = ${y}" + someInteraction
                .map(x => s" AND z_state = ${x}")
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
              "SELECT address, MAX(z_state) FROM cartesian WHERE x_fellowship = 1 AND y_contract = 1  group by x_fellowship, y_contract"
            )
          )
          lockAddress <- Sync[F].delay(rs.getString("address"))
        } yield lockAddress
      }

      override def initWalletState(
        networkId: Int,
        ledgerId:  Int,
        vk:        VerificationKey
      ): F[Unit] = {
        import co.topl.brambl.common.ContainsEvidence.Ops
        import TransactionBuilderApi.implicits._
        import co.topl.brambl.common.ContainsImmutable.instances._
        import cats.implicits._
        connection.use { conn =>
          for {
            stmnt <- Sync[F].delay(conn.createStatement())
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE TABLE IF NOT EXISTS cartesian (id INTEGER PRIMARY KEY," +
                " x_fellowship INTEGER NOT NULL, y_contract INTEGER NOT NULL, z_state INTEGER NOT NULL, " +
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
                "CREATE TABLE IF NOT EXISTS templates (contract TEXT NOT NULL," +
                " y_contract INTEGER PRIMARY KEY ASC,  lock TEXT NOT NULL)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE TABLE IF NOT EXISTS verification_keys (x_fellowship INTEGER NOT NULL," +
                " y_contract INTEGER NOT NULL, vks TEXT NOT NULL, PRIMARY KEY (x_fellowship, y_contract))"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE UNIQUE INDEX IF NOT EXISTS contract_names_idx ON templates (contract)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE INDEX IF NOT EXISTS fellowship_names_idx ON fellowships (fellowship)"
              )
            )
            _ <- Sync[F].delay(
              stmnt.execute(
                "CREATE UNIQUE INDEX IF NOT EXISTS cartesian_coordinates ON cartesian (x_fellowship, y_contract, z_state)"
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
                s"INSERT INTO templates (contract, y_contract, lock) VALUES ('default', 1, '${encodeLockTemplate(defaultTemplate).toString}')"
              )
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                s"INSERT INTO templates (contract, y_contract, lock) VALUES ('genesis', 2, '${encodeLockTemplate(genesisTemplate).toString}')"
              )
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                s"INSERT INTO verification_keys (x_fellowship, y_contract, vks) VALUES (1, 1, '${List(Encoding.encodeToBase58(vk.toByteArray)).asJson.toString}')"
              )
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                s"INSERT INTO verification_keys (x_fellowship, y_contract, vks) VALUES (0, 2, '${List[String]().asJson.toString}')"
              )
            )
            defaultSignatureLock <- getLock("self", "default", 1).map(_.get)
            signatureLockAddress = LockAddress(
              networkId,
              ledgerId,
              LockId(Lock().withPredicate(defaultSignatureLock.getPredicate).sizedEvidence.digest.value)
            )
            childVk           <- walletApi.deriveChildVerificationKey(vk, 1)
            genesisHeightLock <- getLock("nofellowship", "genesis", 1).map(_.get)
            heightLockAddress = LockAddress(
              networkId,
              ledgerId,
              LockId(Lock().withPredicate(genesisHeightLock.getPredicate).sizedEvidence.digest.value)
            )
            _ <- Sync[F].delay(
              stmnt.executeUpdate(
                "INSERT INTO cartesian (x_fellowship, y_contract, z_state, lock_predicate, address, routine, vk) VALUES (1, 1, 1, '" +
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
                "INSERT INTO cartesian (x_fellowship, y_contract, z_state, lock_predicate, address) VALUES (0, 2, 1, '" +
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

      // TODO: We are not yet supporting Digest Propositions in brambl-cli.
      override def getPreimage(
        digestProposition: Proposition.Digest
      ): F[Option[Preimage]] = Sync[F].delay(
        None
      )

      override def addEntityVks(
        fellowship:    String,
        contract: String,
        fellows:  List[String]
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
              s"SELECT y_contract FROM templates WHERE contract = '${contract}'"
            )
          )
          y <- Sync[F].delay(rs.getInt("y_contract"))
          statement =
            s"INSERT INTO verification_keys (x_fellowship, y_contract, vks) VALUES (${x}, ${y}, " +
              s"'${fellows.asJson.toString}')"
          _ <- Sync[F].blocking(
            stmnt.executeUpdate(statement)
          )
        } yield ()
      }

      override def getEntityVks(
        fellowship:    String,
        contract: String
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
              s"SELECT y_contract FROM templates WHERE contract = '${contract}'"
            )
          )
          y <- Sync[F].delay(rs.getInt("y_contract"))
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT vks FROM verification_keys WHERE x_fellowship = ${x} AND y_contract = ${y}"
            )
          )
          vks <- Sync[F].delay(rs.getString("vks"))
        } yield
          if (!rs.next()) None
          else parse(vks).toOption.flatMap(_.as[List[String]].toOption)
      }

      override def addNewLockTemplate(
        contract:     String,
        lockTemplate: LockTemplate[F]
      ): F[Unit] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          // FIXME: do not use max, use autoincrement
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT MAX(y_contract) as y_index FROM templates"
            )
          )
          y <- Sync[F].delay(rs.getInt("y_index"))
          statement =
            s"INSERT INTO templates (contract, y_contract, lock) VALUES ('${contract}', ${y + 1}, '${encodeLockTemplate(lockTemplate).toString}')"
          _ <- Sync[F].blocking(
            stmnt.executeUpdate(statement)
          )
        } yield ()
      }

      override def getLockTemplate(
        contract: String
      ): F[Option[LockTemplate[F]]] = connection.use { conn =>
        for {
          stmnt <- Sync[F].blocking(conn.createStatement())
          rs <- Sync[F].blocking(
            stmnt.executeQuery(
              s"SELECT lock FROM templates WHERE contract = '${contract}'"
            )
          )
          lockStr <- Sync[F].delay(rs.getString("lock"))
        } yield
          if (!rs.next()) None
          else
            parse(lockStr).toOption.flatMap(decodeLockTemplate[F](_).toOption)
      }

      override def getLock(
        fellowship:           String,
        contract:        String,
        nextInteraction: Int
      ): F[Option[Lock]] = for {
        changeTemplate <- getLockTemplate(contract)
        entityVks <- getEntityVks(fellowship, contract)
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
    }
}
