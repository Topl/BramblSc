package co.topl.brambl.servicekit

import cats.effect.IO
import co.topl.brambl.builders.locks.{LockTemplate, PropositionTemplate}
import co.topl.brambl.codecs.LockTemplateCodecs.encodeLockTemplate
import co.topl.brambl.dataApi.{TemplateStorageAlgebra, WalletTemplate}
import munit.CatsEffectSuite
import co.topl.brambl.constants.NetworkConstants

class TemplateStorageApiSpec extends CatsEffectSuite with BaseSpec {

  val contractApi: TemplateStorageAlgebra[IO] = TemplateStorageApi.make[IO](dbConnection)

  testDirectory.test("addTemplate then findTemplates") { _ =>
    val lockTemplate: LockTemplate[IO] =
      LockTemplate.PredicateTemplate[IO](List(PropositionTemplate.HeightTemplate[IO]("chain", 0, 100)), 1)
    val lockTemplateStr = encodeLockTemplate(lockTemplate).noSpaces
    val contract = WalletTemplate(3, "testTemplate", lockTemplateStr)
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair)
        _         <- contractApi.addTemplate(contract)
        templates <- contractApi.findTemplates()
      } yield templates.length == 3 && templates.last == contract,
      true
    )
  }
}
