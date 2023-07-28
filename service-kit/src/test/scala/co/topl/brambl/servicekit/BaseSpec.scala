package co.topl.brambl.servicekit

import munit.CatsEffectSuite

import cats.effect.IO
import co.topl.brambl.dataApi.WalletKeyApiAlgebra
import co.topl.brambl.wallet.WalletApi
import scala.concurrent.duration.Duration
import java.io.File
import java.nio.file.{Files, Path, Paths}

trait BaseSpec extends CatsEffectSuite {
  override val munitTimeout: Duration = Duration(180, "s")

  val walletKeyApi: WalletKeyApiAlgebra[IO] = WalletKeyApi.make[IO]()
  val walletApi: WalletApi[IO] = WalletApi.make[IO](walletKeyApi)

  val TEST_DIR = "./tmp"
  val testDir: File = Paths.get(TEST_DIR).toFile

  private def removeDir() =
    if (testDir.exists()) {
      Paths.get(TEST_DIR).toFile.listFiles().map(_.delete()).mkString("\n")
      Files.deleteIfExists(Paths.get(TEST_DIR))
    }

  val testDirectory: FunFixture[Path] = FunFixture[Path](
    setup = { _ =>
      removeDir()
      Files.createDirectory(Paths.get(TEST_DIR))
    },
    teardown = { _ => removeDir() }
  )
}
