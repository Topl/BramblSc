package co.topl.brambl.wallet

import cats.Id
import co.topl.brambl.{MockDataApi, MockHelpers}
import co.topl.crypto.encryption.VaultStore
import co.topl.crypto.generation.mnemonic.MnemonicSizes
import quivr.models.KeyPair

class WalletApiSpec extends munit.FunSuite with MockHelpers {

  test(
    "createNewWallet: Creating a new wallet creates VaultStore that contains a Topl Main Key and a Mnemonic (default length 12)"
  ) {
    val walletApi = WalletApi.make[Id](MockDataApi)
    val password = "password".getBytes
    val res = walletApi.createNewWallet(password)
    assert(res.isRight)
    assert(res.toOption.get.length == 12)
    val vs = MockDataApi.getMainKeyVaultStore().toOption
    assert(vs.isDefined)
    val mainKey = vs.flatMap(VaultStore.decodeCipher[Id](_, password).toOption).map(KeyPair.parseFrom)
    assert(mainKey.isDefined)
    assert(mainKey.get.vk.vk.extendedEd25519.isDefined)
    assert(mainKey.get.sk.sk.extendedEd25519.isDefined)
  }

  test("createNewWallet: Specifying a valid mnemonic length returns a mnemonic of correct length") {
    val walletApi = WalletApi.make[Id](MockDataApi)
    val password = "password".getBytes
    val res = walletApi.createNewWallet(password, mLen = MnemonicSizes.words24)
    assert(res.isRight)
    assert(res.toOption.get.length == 24)
  }
}
