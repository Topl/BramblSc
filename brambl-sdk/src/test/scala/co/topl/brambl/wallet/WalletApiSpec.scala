package co.topl.brambl.wallet

import cats.Id
import co.topl.brambl.{MockDataApi, MockHelpers}
import co.topl.crypto.encryption.VaultStore
import co.topl.crypto.generation.mnemonic.MnemonicSizes
import quivr.models.KeyPair
import cats.arrow.FunctionK

class WalletApiSpec extends munit.FunSuite with MockHelpers {

  test(
    "createAndSaveNewWallet: Creating a new wallet creates VaultStore that contains a Topl Main Key and a Mnemonic (default length 12)"
  ) {
    val walletApi = WalletApi.make[Id](MockDataApi)
    val password = "password".getBytes
    // import implicit instances for natural transformations
    import cats.arrow._
    import cats.~>
    implicit val idToId = FunctionK.id[Id]
    val res = walletApi.createAndSaveNewWallet[Id](password)
    assert(res.isRight)
    assert(res.toOption.get.mnemonic.length == 12)
    val vs = res.toOption.get.mainKeyVaultStore
    val vsStored = MockDataApi.getMainKeyVaultStore().toOption
    assert(vsStored.isDefined)
    assert(vsStored.get == vs)
    val mainKey = VaultStore.decodeCipher[Id](vs, password).toOption.map(KeyPair.parseFrom)
    assert(mainKey.isDefined)
    assert(mainKey.get.vk.vk.extendedEd25519.isDefined)
    assert(mainKey.get.sk.sk.extendedEd25519.isDefined)
  }

  test("createNewWallet: Specifying a valid mnemonic length returns a mnemonic of correct length") {
    val walletApi = WalletApi.make[Id](MockDataApi)
    val password = "password".getBytes
    val res = walletApi.createNewWallet(password, mLen = MnemonicSizes.words24)
    assert(res.isRight)
    assert(res.toOption.get.mnemonic.length == 24)
  }

  test("saveWallet: specifying a name other than 'default' saves the wallet under that name") {
    val walletApi = WalletApi.make[Id](MockDataApi)
    val w1 = walletApi.createNewWallet("password1".getBytes).toOption.get.mainKeyVaultStore
    val w2 = walletApi.createNewWallet("password2".getBytes).toOption.get.mainKeyVaultStore
    assert(w1 != w2)
    val res1 = walletApi.saveWallet(w1, "w1")
    val res2 = walletApi.saveWallet(w2, "w2")
    assert(res1.isRight)
    assert(res2.isRight)
    val stored1 = MockDataApi.getMainKeyVaultStore("w1").toOption
    assert(stored1.isDefined)
    assert(stored1.get == w1)
    val stored2 = MockDataApi.getMainKeyVaultStore("w2").toOption
    assert(stored2.isDefined)
    assert(stored2.get == w2)
  }
}
