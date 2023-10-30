package co.topl.brambl.servicekit

import cats.effect.IO
import co.topl.brambl.dataApi.{FellowshipStorageAlgebra, WalletFellowship}
import munit.CatsEffectSuite
import co.topl.brambl.constants.NetworkConstants

class FellowshipStorageApiSpec extends CatsEffectSuite with BaseSpec {

  val fellowshipApi: FellowshipStorageAlgebra[IO] = FellowshipStorageApi.make[IO](dbConnection)

  testDirectory.test("addFellowship then findFellowships") { _ =>
    val fellowship = WalletFellowship(2, "testFellowship")
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        _       <- fellowshipApi.addFellowship(fellowship)
        fellowships <- fellowshipApi.findFellowships()
      } yield fellowships.length == 3 && fellowships.last == fellowship,
      true
    )
  }
}
