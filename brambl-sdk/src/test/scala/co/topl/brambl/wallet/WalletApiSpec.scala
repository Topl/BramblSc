package co.topl.brambl.wallet

import cats.Id
import co.topl.brambl.{MockDataApi, MockHelpers}
import co.topl.crypto.encryption.VaultStore
import co.topl.crypto.generation.mnemonic.MnemonicSizes
import quivr.models.KeyPair
import cats.arrow.FunctionK
import co.topl.brambl.models.Indices
import co.topl.crypto.signing.ExtendedEd25519
import io.circe.syntax.EncoderOps
import co.topl.crypto.encryption.VaultStore.Codecs._
import co.topl.crypto.generation.mnemonic.EntropyFailures.PhraseToEntropyFailure
import co.topl.crypto.generation.mnemonic.PhraseFailures.InvalidWordLength

class WalletApiSpec extends munit.FunSuite with MockHelpers {
  implicit val idToId: FunctionK[Id, Id] = FunctionK.id[Id]
  val walletApi: WalletApi[Id] = WalletApi.make[Id](MockDataApi)
  val testMsg: Array[Byte] = "test message".getBytes

  // Runs after each individual test.
  override def afterEach(context: AfterEach): Unit =
    MockDataApi.mainKeyVaultStoreInstance = Map() // Reset the MockDataApi persisted data

  test(
    "createAndSaveNewWallet: Creating a new wallet creates VaultStore that contains a Topl Main Key and a Mnemonic (default length 12)"
  ) {
    val password = "password".getBytes
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
    val res = walletApi.createNewWallet("password".getBytes, mLen = MnemonicSizes.words24)
    assert(res.isRight)
    assert(res.toOption.get.mnemonic.length == 24)
  }

  test("saveWallet and loadWallet: specifying a name other than 'default' saves the wallet under that name") {
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

  test(
    "loadWallet: if the wallet with the name does not exist, the correct error is returned"
  ) {
    val loadRes = walletApi.loadWallet("w1")
    assert(loadRes.isLeft)
    assert(loadRes.left.toOption.get == WalletApi.FailedToLoadWallet(MockDataApi.MainKeyVaultStoreNotInitialized))
  }

  test("extractMainKey: ExtendedEd25519 Topl Main Key is returned") {
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
    val signingInstance: ExtendedEd25519 = new ExtendedEd25519
    val password = "password".getBytes
    val res1 = walletApi.createAndSaveNewWallet[Id](password, passphrase = Some("passphrase1"), name = "w1")
    val res2 = walletApi.createAndSaveNewWallet[Id](password, passphrase = Some("passphrase2"), name = "w2")
    assert(res1.isRight)
    assert(res2.isRight)
    val kp1Either = walletApi.loadAndExtractMainKey[Id](password, "w1")
    val kp2Either = walletApi.loadAndExtractMainKey[Id](password, "w2")
    assert(kp1Either.isRight)
    assert(kp2Either.isRight)
    val kp1 = kp1Either.toOption.get
    val kp2 = kp2Either.toOption.get
    assert(kp1.vk.vk.extendedEd25519.isDefined)
    assert(kp1.sk.sk.extendedEd25519.isDefined)
    assert(kp2.vk.vk.extendedEd25519.isDefined)
    assert(kp2.sk.sk.extendedEd25519.isDefined)
    val signature1 = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(kp1).signingKey, testMsg)
    val signature2 = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(kp2).signingKey, testMsg)
    assert(signingInstance.verify(signature1, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(kp1).verificationKey))
    assert(signingInstance.verify(signature2, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(kp2).verificationKey))
    assert(!signingInstance.verify(signature1, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(kp2).verificationKey))
    assert(!signingInstance.verify(signature2, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(kp1).verificationKey))
  }

  test(
    "createAndSaveNewWallet: If the wallet is successfully created but not saved, the correct error is returned"
  ) {
    // DataApi mocked "error" to return an error when saving the wallet
    val res = walletApi.createAndSaveNewWallet[Id]("password".getBytes, name = "error")
    assert(res.isLeft)
    assert(res.left.toOption.get == WalletApi.FailedToSaveWallet(MockDataApi.MainKeyVaultSaveFailure))
  }

  test("deriveChildKeys: Verify deriving path 4'/4/4 produces a valid child key pair") {
    val signingInstance: ExtendedEd25519 = new ExtendedEd25519
    val password = "password".getBytes
    val vaultStore = walletApi.createNewWallet(password).toOption.map(_.mainKeyVaultStore).get
    val mainKey = walletApi.extractMainKey(vaultStore, password).toOption.get
    val idx = Indices(4, 4, 4)
    val childKey = walletApi.deriveChildKeys(mainKey, idx)
    val signature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(childKey).signingKey, testMsg)
    assert(signingInstance.verify(signature, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(childKey).verificationKey))
  }

  test(
    "buildMainKeyVaultStore: Build a VaultStore for a main key encrypted with a password."
  ) {
    val mainKey = "dummyKeyPair".getBytes
    val password = "password".getBytes
    // Using the same password should return the same VaultStore
    val v1 = walletApi.buildMainKeyVaultStore(mainKey, password)
    val v2 = walletApi.buildMainKeyVaultStore(mainKey, password)
    assert(v1 == v2)
    assert(
      VaultStore.decodeCipher(v1, password).toOption.get sameElements VaultStore.decodeCipher(v2, password).toOption.get
    )
    // Using a different password should decode the VaultStore to the same key
    val v3 = walletApi.buildMainKeyVaultStore(mainKey, "password2".getBytes)
    assert(v1 != v3)
    assert(
      VaultStore.decodeCipher(v1, password).toOption.get sameElements VaultStore.decodeCipher(v2, password).toOption.get
    )
  }

  test(
    "deleteWallet: Deleting a wallet when a wallet of that name does not exist > Error"
  ) {
    val deleteRes = walletApi.deleteWallet("name")
    assert(deleteRes.isLeft)
    assert(deleteRes.left.toOption.get == WalletApi.FailedToDeleteWallet(MockDataApi.MainKeyVaultDeleteFailure))
  }

  test(
    "deleteWallet: Deleting a wallet > Verify wallet no longer exists at the specified name"
  ) {
    val saveRes = walletApi.createAndSaveNewWallet[Id]("password".getBytes, name = "name")
    assert(saveRes.isRight)
    val beforeDelete = walletApi.loadWallet("name")
    assert(beforeDelete.isRight)
    val deleteRes = walletApi.deleteWallet("name")
    assert(deleteRes.isRight)
    val afterDelete = walletApi.loadWallet("name")
    assert(afterDelete.isLeft)
    assert(afterDelete.left.toOption.get == WalletApi.FailedToLoadWallet(MockDataApi.MainKeyVaultStoreNotInitialized))
  }

  test(
    "updateWallet: Updating a wallet when a wallet of that name does not exist > Error"
  ) {
    val vs = walletApi.buildMainKeyVaultStore("dummyKeyPair".getBytes, "password".getBytes)
    val w1 = walletApi.updateWallet(vs, "name")
    assert(w1.isLeft)
    assert(w1.left.toOption.get == WalletApi.FailedToUpdateWallet(MockDataApi.MainKeyVaultStoreNotInitialized))
  }

  test(
    "updateWallet: Updating a wallet > Verify old wallet no longer exists at the specified name"
  ) {
    val password = "password".getBytes
    val oldWallet = walletApi.createAndSaveNewWallet[Id](password, name = "w1")
    assert(oldWallet.isRight)
    // same password, different key
    val newWallet = walletApi.buildMainKeyVaultStore("dummyKeyPair".getBytes, password)
    val updateRes = walletApi.updateWallet(newWallet, "w1")
    assert(updateRes.isRight)
    val loadedWalletRes = walletApi.loadWallet("w1")
    assert(loadedWalletRes.isRight)
    val loadedWallet = loadedWalletRes.toOption.get
    assert(loadedWallet != oldWallet.toOption.get.mainKeyVaultStore)
    assert(loadedWallet == newWallet)
  }

  test(
    "updateWalletPassword: Updating a wallet password > Same key stored but with a different password"
  ) {
    val oldPassword = "oldPassword".getBytes
    val newPassword = "newPassword".getBytes
    val oldWallet = walletApi.createAndSaveNewWallet[Id](oldPassword)
    assert(oldWallet.isRight)
    val oldVaultStore = oldWallet.toOption.get.mainKeyVaultStore
    val mainKey = walletApi.extractMainKey(oldVaultStore, oldPassword).toOption.get
    // verify old wallet has been saved
    assert(walletApi.loadWallet().toOption.get == oldVaultStore)
    val updateRes = walletApi.updateWalletPassword[Id](oldPassword, newPassword)
    assert(updateRes.isRight)
    val newVaultStore = updateRes.toOption.get
    val loadedWallet = walletApi.loadWallet().toOption.get
    // Verify the old wallet has been replaced
    assert(loadedWallet != oldVaultStore)
    assert(loadedWallet == newVaultStore)
    // Verify the old password does not work for the loaded wallet
    val decodeOldPassword = walletApi.extractMainKey(loadedWallet, oldPassword)
    assert(decodeOldPassword.isLeft)
    assert(decodeOldPassword.left.toOption.get == WalletApi.FailedToDecodeWallet(VaultStore.InvalidMac))
    // Verify the new password works for the loaded wallet
    val decodeNewPassword = walletApi.extractMainKey(loadedWallet, newPassword)
    assert(decodeNewPassword.isRight)
    assert(decodeNewPassword.toOption.get == mainKey)
  }

  test(
    "updateWalletPassword: Failure saving > Wallet is accessible with the old password"
  ) {
    val password = "password".getBytes
    val oldVaultStore = walletApi.createNewWallet(password).toOption.get.mainKeyVaultStore
    // manually save the wallet to the mock data api with the error name
    MockDataApi.mainKeyVaultStoreInstance += ("error" -> oldVaultStore.asJson)
    val updateRes = walletApi.updateWalletPassword[Id](password, "newPassword".getBytes, "error")
    assert(updateRes.isLeft)
    assert(updateRes.left.toOption.get == WalletApi.FailedToUpdateWallet(MockDataApi.MainKeyVaultSaveFailure))
    // verify the wallet is still accessible with the old password
    val loadedWallet = walletApi.loadAndExtractMainKey[Id](password, "error")
    assert(loadedWallet.isRight)
  }

  test("importWallet: import using mnemonic from createNewWallet > Same Main Key") {
    val oldPassword = "old-password".getBytes
    val wallet = walletApi.createNewWallet(oldPassword).toOption.get
    val mnemonic = wallet.mnemonic
    val mainKey = walletApi.extractMainKey(wallet.mainKeyVaultStore, oldPassword).toOption.get
    val newPassword = "new-password".getBytes
    val importedWallet = walletApi.importWallet(mnemonic, newPassword)
    assert(importedWallet.isRight)
    assert(wallet.mainKeyVaultStore != importedWallet.toOption.get) // Should be different due to password
    val importedMainKey = walletApi.extractMainKey(importedWallet.toOption.get, newPassword)
    assert(importedMainKey.isRight)
    val testMainKey = importedMainKey.toOption.get
    // Verify the main key is the same
    assert(mainKey == testMainKey)
    val signingInstance: ExtendedEd25519 = new ExtendedEd25519
    val signature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(mainKey).signingKey, testMsg)
    val testSignature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(testMainKey).signingKey, testMsg)
    assert(java.util.Arrays.equals(signature, testSignature))

    assert(signingInstance.verify(signature, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(testMainKey).verificationKey))
    assert(signingInstance.verify(testSignature, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(mainKey).verificationKey))

  }

  test("importWallet: an invalid mnemonic produces correct error") {
    val password = "password".getBytes
    val wallet = walletApi.createNewWallet(password).toOption.get
    val mnemonic = wallet.mnemonic :+ "extraWord"
    val importedWallet = walletApi.importWallet(mnemonic, password)
    assert(importedWallet.isLeft)
    assert(
      importedWallet.left.toOption.get == WalletApi.FailedToInitializeWallet(PhraseToEntropyFailure(InvalidWordLength))
    )
  }

  test("importWalletAndSave: verify a save failure returns the correct error") {
    val password = "password".getBytes
    val wallet = walletApi.createNewWallet(password).toOption.get
    val importedWallet = walletApi.importWalletAndSave[Id](wallet.mnemonic, password, name = "error")
    assert(importedWallet.isLeft)
    assert(importedWallet.left.toOption.get == WalletApi.FailedToSaveWallet(MockDataApi.MainKeyVaultSaveFailure))
  }

  test("recoverWallet: TBD when recovery is fleshed out".ignore) {}
}
