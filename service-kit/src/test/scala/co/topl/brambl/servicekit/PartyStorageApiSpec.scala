package co.topl.brambl.servicekit

import cats.effect.IO
import co.topl.brambl.dataApi.{PartyStorageAlgebra, WalletEntity}
import munit.CatsEffectSuite

class PartyStorageApiSpec extends CatsEffectSuite with BaseSpec {

  val partyApi: PartyStorageAlgebra[IO] = PartyStorageApi.make[IO](dbConnection)

  testDirectory.test("addParty then findParties") { _ =>
    val party = WalletEntity(2, "testParty")
    assertIO(
      for {
        init    <- walletStateApi.initWalletState(mockMainKeyPair.vk)
        _       <- partyApi.addParty(party)
        parties <- partyApi.findParties()
      } yield parties.length == 3 && parties.last == party,
      true
    )
  }
}
