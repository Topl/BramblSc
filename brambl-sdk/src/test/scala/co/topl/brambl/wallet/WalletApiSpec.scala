package co.topl.brambl.wallet

import cats.Id
import co.topl.brambl.{MockDataApi, MockHelpers}
import co.topl.crypto.encryption.VaultStore
import co.topl.crypto.generation.mnemonic.MnemonicSizes
import quivr.models.KeyPair
import cats.arrow.FunctionK
import co.topl.brambl.models.Indices
import co.topl.crypto.signing.ExtendedEd25519

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

  test("saveWallet and loadWallet: specifying a name other than 'default' saves the wallet under that name") {
    val walletApi = WalletApi.make[Id](MockDataApi)
    val w1 = walletApi.createNewWallet("password1".getBytes).toOption.get.mainKeyVaultStore
    val w2 = walletApi.createNewWallet("password2".getBytes).toOption.get.mainKeyVaultStore
    assert(w1 != w2)
    val res1 = walletApi.saveWallet(w1, "w1")
    val res2 = walletApi.saveWallet(w2, "w2")
    assert(res1.isRight)
    assert(res2.isRight)
    val stored1 = walletApi.loadWallet("w1").toOption
    assert(stored1.isDefined)
    assert(stored1.get == w1)
    val stored2 = walletApi.loadWallet("w2").toOption
    assert(stored2.isDefined)
    assert(stored2.get == w2)
  }

  test("extractMainKey: ExtendedEd25519 Topl Main Key is returned") {
    val walletApi = WalletApi.make[Id](MockDataApi)
    val password = "password".getBytes
    val vaultStore = walletApi.createNewWallet(password).toOption.map(_.mainKeyVaultStore).get
    val mainKeyOpt = walletApi.extractMainKey(vaultStore, password)
    assert(mainKeyOpt.isRight)
    val mainKey = mainKeyOpt.toOption.get
    assert(mainKey.vk.vk.extendedEd25519.isDefined)
    assert(mainKey.sk.sk.extendedEd25519.isDefined)
    val testMsg = "test message".getBytes
    val signingInstance = new ExtendedEd25519
    val signature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(mainKey).signingKey, testMsg)
    assert(signingInstance.verify(signature, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(mainKey).verificationKey))
  }

  test(
    "createAndSaveNewWallet and loadAndExtractMainKey: specifying a name other than 'default' extracts the Topl Main Key under that name"
  ) {
    import cats.arrow._
    import cats.~>
    implicit val idToId = FunctionK.id[Id]
    val walletApi = WalletApi.make[Id](MockDataApi)
    val password = "password".getBytes
    val res1 = walletApi.createAndSaveNewWallet[Id](password, passphrase = Some("passphrase1"), name = "w3")
    val res2 = walletApi.createAndSaveNewWallet[Id](password, passphrase = Some("passphrase2"), name = "w4")
    assert(res1.isRight)
    assert(res2.isRight)
    val kp1Either = walletApi.loadAndExtractMainKey[Id](password, "w3")
    val kp2Either = walletApi.loadAndExtractMainKey[Id](password, "w4")
    assert(kp1Either.isRight)
    assert(kp2Either.isRight)
    val kp1 = kp1Either.toOption.get
    val kp2 = kp2Either.toOption.get
    assert(kp1.vk.vk.extendedEd25519.isDefined)
    assert(kp1.sk.sk.extendedEd25519.isDefined)
    assert(kp2.vk.vk.extendedEd25519.isDefined)
    assert(kp2.sk.sk.extendedEd25519.isDefined)
    val testMsg = "test message".getBytes
    val signingInstance = new ExtendedEd25519
    val signature1 = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(kp1).signingKey, testMsg)
    val signature2 = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(kp2).signingKey, testMsg)
    assert(signingInstance.verify(signature1, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(kp1).verificationKey))
    assert(signingInstance.verify(signature2, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(kp2).verificationKey))
    assert(!signingInstance.verify(signature1, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(kp2).verificationKey))
    assert(!signingInstance.verify(signature2, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(kp1).verificationKey))

  }

  test("deriveChildKeys: Verify deriving path 4'/4/4 produces a valid child key pair") {
    val walletApi = WalletApi.make[Id](MockDataApi)
    val password = "password".getBytes
    val vaultStore = walletApi.createNewWallet(password).toOption.map(_.mainKeyVaultStore).get
    val mainKey = walletApi.extractMainKey(vaultStore, password).toOption.get
    val idx = Indices(4, 4, 4)
    val childKey = walletApi.deriveChildKeys(mainKey, idx)
    val testMsg = "test message".getBytes
    val signingInstance = new ExtendedEd25519
    val signature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(childKey).signingKey, testMsg)
    assert(signingInstance.verify(signature, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(childKey).verificationKey))
  }
}
