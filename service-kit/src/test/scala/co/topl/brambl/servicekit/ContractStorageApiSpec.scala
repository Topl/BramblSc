package co.topl.brambl.servicekit

import cats.effect.IO
import co.topl.brambl.builders.locks.{LockTemplate, PropositionTemplate}
import co.topl.brambl.codecs.LockTemplateCodecs.encodeLockTemplate
import co.topl.brambl.dataApi.{ContractStorageAlgebra, WalletContract}
import munit.CatsEffectSuite
import co.topl.brambl.constants.NetworkConstants

class ContractStorageApiSpec extends CatsEffectSuite with BaseSpec {

  val contractApi: ContractStorageAlgebra[IO] = ContractStorageApi.make[IO](dbConnection)

  testDirectory.test("addContract then findContracts") { _ =>
    val lockTemplate: LockTemplate[IO] =
      LockTemplate.PredicateTemplate[IO](List(PropositionTemplate.HeightTemplate[IO]("chain", 0, 100)), 1)
    val lockTemplateStr = encodeLockTemplate(lockTemplate).noSpaces
    val contract = WalletContract(3, "testContract", lockTemplateStr)
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        _         <- contractApi.addContract(contract)
        contracts <- contractApi.findContracts()
      } yield contracts.length == 3 && contracts.last == contract,
      true
    )
  }
}
