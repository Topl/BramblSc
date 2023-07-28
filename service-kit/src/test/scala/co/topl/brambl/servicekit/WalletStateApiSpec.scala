package co.topl.brambl.servicekit

import cats.effect.IO
import cats.effect.kernel.Resource
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.constants.NetworkConstants._
import co.topl.brambl.dataApi.WalletStateAlgebra
import co.topl.brambl.wallet.WalletApi.cryptoToPbKeyPair
import co.topl.crypto.generation.KeyInitializer.Instances.extendedEd25519Initializer
import co.topl.crypto.signing.{ExtendedEd25519, KeyPair}
import munit.CatsEffectSuite

import java.sql.Connection

class WalletStateApiSpec extends CatsEffectSuite with WalletStateResource with BaseSpec {

  val DB_FILE = s"$TEST_DIR/wallet.db"

  val dbConnection: Resource[IO, Connection] = walletResource(DB_FILE)

  val transactionBuilderApi: TransactionBuilderApi[IO] =
    TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)
  val walletStateApi: WalletStateAlgebra[IO] = WalletStateApi.make[IO](dbConnection, transactionBuilderApi, walletApi)

  private def mockMainKeyPair = {
    implicit val extendedEd25519Instance: ExtendedEd25519 = new ExtendedEd25519
    val sk = extendedEd25519Initializer.random()
    cryptoToPbKeyPair(KeyPair(sk, extendedEd25519Instance.getVerificationKey(sk)))
  }

  testDirectory.test("initWalletState") { _ =>
    assertIO(
      for {
        init <- walletStateApi.initWalletState(mockMainKeyPair.vk)
        partyCount <- dbConnection.use { conn =>
          for {
            stmt <- IO.delay(conn.createStatement())
            rs <- IO.blocking(
              stmt.executeQuery("SELECT COUNT(*) as res FROM parties WHERE party IN ('noparty', 'self')")
            )
            count <- IO.delay(rs.getInt("res"))
          } yield count
        }
        contractCount <- dbConnection.use { conn =>
          for {
            stmt <- IO.delay(conn.createStatement())
            rs <- IO.blocking(
              stmt.executeQuery("SELECT COUNT(*) as res FROM contracts WHERE contract IN ('default', 'genesis')")
            )
            count <- IO.delay(rs.getInt("res"))
          } yield count
        }
        vkCount <- dbConnection.use { conn =>
          for {
            stmt  <- IO.delay(conn.createStatement())
            rs    <- IO.blocking(stmt.executeQuery("SELECT COUNT(*) as res FROM verification_keys"))
            count <- IO.delay(rs.getInt("res"))
          } yield count
        }
        cartesianCount <- dbConnection.use { conn =>
          for {
            stmt  <- IO.delay(conn.createStatement())
            rs    <- IO.blocking(stmt.executeQuery("SELECT COUNT(*) as res FROM cartesian"))
            count <- IO.delay(rs.getInt("res"))
          } yield count
        }
      } yield partyCount == 2 && contractCount == 2 && vkCount == 2 && cartesianCount == 2,
      true
    )
  }
}
