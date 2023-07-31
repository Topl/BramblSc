package co.topl.brambl.servicekit

import cats.effect.IO
import co.topl.brambl.dataApi.{PartyStorageAlgebra, WalletEntity}
import munit.CatsEffectSuite
import co.topl.brambl.constants.NetworkConstants

class PartyStorageApiSpec extends CatsEffectSuite with BaseSpec {

  val partyApi: PartyStorageAlgebra[IO] = PartyStorageApi.make[IO](dbConnection)

  testDirectory.test("addParty then findParties") { _ =>
    val party = WalletEntity(2, "testParty")
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        _       <- partyApi.addParty(party)
        parties <- partyApi.findParties()
      } yield parties.length == 3 && parties.last == party,
      true
    )
  }
}
