package co.topl.brambl.wallet

import cats.Id
import co.topl.brambl.{MockHelpers, MockWalletKeyApi}
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

class WalletApiSpec extends munit.CatsEffectSuite with MockHelpers {
  implicit val idToId: FunctionK[F, F] = FunctionK.id[F]
  val walletApi: WalletApi[F] = WalletApi.make[F](MockWalletKeyApi)
  val testMsg: Array[Byte] = "test message".getBytes

  // Runs after each individual test.
  override def afterEach(context: AfterEach): Unit =
    MockWalletKeyApi.mainKeyVaultStoreInstance = Map() // Reset the MockDataApi persisted data

  test(
    "createAndSaveNewWallet: Creating a new wallet creates VaultStore that contains a Topl Main Key and a Mnemonic (default length 12)"
  ) {
    val password = "password".getBytes
    assertIO(
      for {
        res      <- walletApi.createAndSaveNewWallet[F](password)
        vsStored <- MockWalletKeyApi.getMainKeyVaultStore().map(_.toOption)
        vs = res.toOption.get.mainKeyVaultStore
        mainKey <- VaultStore.decodeCipher[F](vs, password).map(_.toOption.map(KeyPair.parseFrom))
      } yield {
        val validMnemonic = res.toOption.get.mnemonic.length == 12
        val validVsStored = (vsStored.isDefined) && (vsStored.get == vs)
        val validMainKey =
          (mainKey.isDefined) && (mainKey.get.vk.vk.extendedEd25519.isDefined) && (mainKey.get.sk.sk.extendedEd25519.isDefined)
        validMnemonic && validVsStored && validMainKey
      },
      true
    )
  }

  test("createNewWallet: Specifying a valid mnemonic length returns a mnemonic of correct length") {
    assertIO(
      walletApi
        .createNewWallet("password".getBytes, mLen = MnemonicSizes.words24)
        .map(_.toOption.get.mnemonic.length == 24),
      true
    )
  }

  test("saveWallet and loadWallet: specifying a name other than 'default' saves the wallet under that name") {
    assertIO(
      for {
        w1      <- walletApi.createNewWallet("password1".getBytes).map(_.toOption.get.mainKeyVaultStore)
        w2      <- walletApi.createNewWallet("password2".getBytes).map(_.toOption.get.mainKeyVaultStore)
        res1    <- walletApi.saveWallet(w1, "w1")
        res2    <- walletApi.saveWallet(w2, "w2")
        stored1 <- walletApi.loadWallet("w1").map(_.toOption)
        stored2 <- walletApi.loadWallet("w2").map(_.toOption)
      } yield {
        val validWalletUnique = w1 != w2
        val validInitialStored = res1.isRight && res2.isRight
        val validLoad1 = (stored1.isDefined) && (stored1.get == w1)
        val validLoad2 = (stored2.isDefined) && (stored2.get == w2)
        validWalletUnique && validInitialStored && validLoad1 && validLoad2
      },
      true
    )
  }

  test(
    "loadWallet: if the wallet with the name does not exist, the correct error is returned"
  ) {
    assertIO(
      walletApi
        .loadWallet("w1")
        .map(_.left.toOption.get == WalletApi.FailedToLoadWallet(MockWalletKeyApi.MainKeyVaultStoreNotInitialized)),
      true
    )
  }

  test("extractMainKey: ExtendedEd25519 Topl Main Key is returned") {
    val password = "password".getBytes
    assertIO(
      for {
        vaultStore <- walletApi.createNewWallet(password).map(_.toOption.get.mainKeyVaultStore)
        mainKeyOpt <- walletApi.extractMainKey(vaultStore, password)
      } yield {
        val mainKey = mainKeyOpt.toOption.get
        val validKey = (mainKey.vk.vk.extendedEd25519.isDefined) && (mainKey.sk.sk.extendedEd25519.isDefined)
        val testMsg = "test message".getBytes
        val signingInstance = new ExtendedEd25519
        val signature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(mainKey).signingKey, testMsg)
        val validSignature =
          signingInstance.verify(signature, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(mainKey).verificationKey)
        validKey && validSignature
      },
      true
    )
  }

  test(
    "createAndSaveNewWallet and loadAndExtractMainKey: specifying a name other than 'default' extracts the Topl Main Key under that name"
  ) {
    val signingInstance: ExtendedEd25519 = new ExtendedEd25519
    val password = "password".getBytes
    assertIO(
      for {
        _         <- walletApi.createAndSaveNewWallet[F](password, passphrase = Some("passphrase1"), name = "w1")
        _         <- walletApi.createAndSaveNewWallet[F](password, passphrase = Some("passphrase2"), name = "w2")
        kp1Either <- walletApi.loadAndExtractMainKey[F](password, "w1")
        kp2Either <- walletApi.loadAndExtractMainKey[F](password, "w2")
      } yield {
        val kp1 = kp1Either.toOption.get
        val kp2 = kp2Either.toOption.get
        val validKeyPairs =
          (kp1.vk.vk.extendedEd25519.isDefined && kp1.sk.sk.extendedEd25519.isDefined) && (kp2.vk.vk.extendedEd25519.isDefined && kp2.sk.sk.extendedEd25519.isDefined)
        val signature1 = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(kp1).signingKey, testMsg)
        val signature2 = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(kp2).signingKey, testMsg)
        val validSignatures = (signingInstance.verify(
          signature1,
          testMsg,
          WalletApi.pbKeyPairToCryotoKeyPair(kp1).verificationKey
        )) && (signingInstance.verify(
          signature2,
          testMsg,
          WalletApi.pbKeyPairToCryotoKeyPair(kp2).verificationKey
        )) && (!signingInstance.verify(
          signature1,
          testMsg,
          WalletApi.pbKeyPairToCryotoKeyPair(kp2).verificationKey
        )) && (!signingInstance.verify(signature2, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(kp1).verificationKey))
        validKeyPairs && validSignatures
      },
      true
    )
  }

  test(
    "createAndSaveNewWallet: If the wallet is successfully created but not saved, the correct error is returned"
  ) {
    // DataApi mocked "error" to return an error when saving the wallet
    assertIO(
      walletApi
        .createAndSaveNewWallet[F]("password".getBytes, name = "error")
        .map(_.left.toOption.get == WalletApi.FailedToSaveWallet(MockWalletKeyApi.MainKeyVaultSaveFailure)),
      true
    )
  }

  test("deriveChildKeys: Verify deriving path 4'/4/4 produces a valid child key pair") {
    val signingInstance: ExtendedEd25519 = new ExtendedEd25519
    val password = "password".getBytes
    val idx = Indices(4, 4, 4)
    assertIO(
      for {
        vaultStore <- walletApi.createNewWallet(password).map(_.toOption.get.mainKeyVaultStore)
        mainKey    <- walletApi.extractMainKey(vaultStore, password).map(_.toOption.get)
        childKey   <- walletApi.deriveChildKeys(mainKey, idx)
      } yield {
        val signature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(childKey).signingKey, testMsg)
        signingInstance.verify(signature, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(childKey).verificationKey)
      },
      true
    )
  }

  test("deriveChildKeysPartial: Verify deriving path 4'/4 produces a valid child key pair") {
    val signingInstance: ExtendedEd25519 = new ExtendedEd25519
    val password = "password".getBytes
    assertIO(
      for {
        vaultStore <- walletApi.createNewWallet(password).map(_.toOption.get.mainKeyVaultStore)
        mainKey    <- walletApi.extractMainKey(vaultStore, password).map(_.toOption.get)
        childKey   <- walletApi.deriveChildKeysPartial(mainKey, 4, 4)
      } yield {
        val signature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(childKey).signingKey, testMsg)
        signingInstance.verify(signature, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(childKey).verificationKey)
      },
      true
    )
  }

  test("deriveChildVerificationKey: Verify deriving path '4' produces a valid child verification key") {
    val signingInstance: ExtendedEd25519 = new ExtendedEd25519
    val password = "password".getBytes

    assertIO(
      for {
        vaultStore               <- walletApi.createNewWallet(password).map(_.toOption.get.mainKeyVaultStore)
        mainKey                  <- walletApi.extractMainKey(vaultStore, password).map(_.toOption.get)
        childKeyExpected         <- walletApi.deriveChildKeys(mainKey, Indices(4, 4, 4))
        childKeyPartial          <- walletApi.deriveChildKeysPartial(mainKey, 4, 4)
        childVerificationKeyTest <- walletApi.deriveChildVerificationKey(childKeyPartial.vk, 4)
      } yield {
        val validVk = childVerificationKeyTest == childKeyExpected.vk
        val signature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(childKeyExpected).signingKey, testMsg)
        validVk && signingInstance
          .verify(signature, testMsg, WalletApi.pbVkToCryptoVk(childVerificationKeyTest.getExtendedEd25519))
      },
      true
    )
  }

  test(
    "buildMainKeyVaultStore: Build a VaultStore for a main key encrypted with a password."
  ) {
    val mainKey = "dummyKeyPair".getBytes
    val password = "password".getBytes
    assertIO(
      for {
        // Using the same password should return the same VaultStore
        v1        <- walletApi.buildMainKeyVaultStore(mainKey, password)
        v2        <- walletApi.buildMainKeyVaultStore(mainKey, password)
        v1Decoded <- VaultStore.decodeCipher(v1, password).map(_.toOption.get)
        v2Decoded <- VaultStore.decodeCipher(v2, password).map(_.toOption.get)
        // Using a different password should decode the VaultStore to the same key
        v3        <- walletApi.buildMainKeyVaultStore(mainKey, "password2".getBytes)
        v3Decoded <- VaultStore.decodeCipher(v3, "password2".getBytes).map(_.toOption.get)
      } yield {
        val validVs2 = (v1 == v2) && (v1Decoded sameElements v2Decoded)
        val validVs3 = (v1 != v3) && (v1Decoded sameElements v3Decoded)
        validVs2 && validVs3
      },
      true
    )
  }

  test(
    "deleteWallet: Deleting a wallet when a wallet of that name does not exist > Error"
  ) {
    assertIO(
      walletApi
        .deleteWallet("name")
        .map(_.left.toOption.get == WalletApi.FailedToDeleteWallet(MockWalletKeyApi.MainKeyVaultDeleteFailure)),
      true
    )
  }

  test(
    "deleteWallet: Deleting a wallet > Verify wallet no longer exists at the specified name"
  ) {
    assertIO(
      for {
        saveRes      <- walletApi.createAndSaveNewWallet[F]("password".getBytes, name = "name")
        beforeDelete <- walletApi.loadWallet("name")
        deleteRes    <- walletApi.deleteWallet("name")
        afterDelete  <- walletApi.loadWallet("name")
      } yield {
        val validDelete = afterDelete.left.toOption.get == WalletApi.FailedToLoadWallet(
          MockWalletKeyApi.MainKeyVaultStoreNotInitialized
        )
        saveRes.isRight && beforeDelete.isRight && deleteRes.isRight && validDelete
      },
      true
    )
  }

  test(
    "updateWallet: Updating a wallet when a wallet of that name does not exist > Error"
  ) {
    assertIO(
      for {
        vs <- walletApi.buildMainKeyVaultStore("dummyKeyPair".getBytes, "password".getBytes)
        w1 <- walletApi.updateWallet(vs, "name")
      } yield w1.isLeft && (w1.left.toOption.get == WalletApi.FailedToUpdateWallet(
        MockWalletKeyApi.MainKeyVaultStoreNotInitialized
      )),
      true
    )
  }

  test(
    "updateWallet: Updating a wallet > Verify old wallet no longer exists at the specified name"
  ) {
    val password = "password".getBytes
    assertIO(
      for {
        oldWallet <- walletApi.createAndSaveNewWallet[F](password, name = "w1")
        // same password, different key
        newWallet       <- walletApi.buildMainKeyVaultStore("dummyKeyPair".getBytes, password)
        updateRes       <- walletApi.updateWallet(newWallet, "w1")
        loadedWalletRes <- walletApi.loadWallet("w1")
      } yield {
        val loadedWallet = loadedWalletRes.toOption.get
        val validLoadedWallet =
          (loadedWallet != oldWallet.toOption.get.mainKeyVaultStore) && (loadedWallet == newWallet)
        oldWallet.isRight && updateRes.isRight && loadedWalletRes.isRight && validLoadedWallet
      },
      true
    )
  }

  test(
    "updateWalletPassword: Updating a wallet password > Same key stored but with a different password"
  ) {
    val oldPassword = "oldPassword".getBytes
    val newPassword = "newPassword".getBytes

    assertIO(
      for {
        oldWallet <- walletApi.createAndSaveNewWallet[F](oldPassword)
        oldVaultStore = oldWallet.toOption.get.mainKeyVaultStore
        mainKey         <- walletApi.extractMainKey(oldVaultStore, oldPassword).map(_.toOption.get)
        loadedOldWallet <- walletApi.loadWallet().map(_.toOption.get)
        updateRes       <- walletApi.updateWalletPassword[F](oldPassword, newPassword)
        loadedWallet    <- walletApi.loadWallet().map(_.toOption.get)
        // Verify the old password does not work for the loaded wallet
        decodeOldPassword <- walletApi.extractMainKey(loadedWallet, oldPassword)
        // Verify the new password works for the loaded wallet
        decodeNewPassword <- walletApi.extractMainKey(loadedWallet, newPassword)
      } yield {
        val validOldWallet = oldWallet.isRight && (loadedOldWallet == oldVaultStore)
        val validUpdate =
          updateRes.isRight && (loadedWallet != oldVaultStore) && (loadedWallet == updateRes.toOption.get)
        val validDecode = (decodeOldPassword.left.toOption.get == WalletApi.FailedToDecodeWallet(
          VaultStore.InvalidMac
        )) && (decodeNewPassword.toOption.get == mainKey)
        validOldWallet && validUpdate && validDecode
      },
      true
    )
  }

  test(
    "updateWalletPassword: Failure saving > Wallet is accessible with the old password"
  ) {
    val password = "password".getBytes
    assertIO(
      for {
        oldVaultStore <- walletApi.createNewWallet(password).map(_.toOption.get.mainKeyVaultStore)
        updateRes <- {
          // manually save the wallet to the mock data api with the error name
          MockWalletKeyApi.mainKeyVaultStoreInstance += ("error" -> oldVaultStore.asJson)
          walletApi.updateWalletPassword[F](password, "newPassword".getBytes, "error")
        }
        // verify the wallet is still accessible with the old password
        loadedWallet <- walletApi.loadAndExtractMainKey[F](password, "error")
      } yield {
        val validUpdate =
          updateRes.left.toOption.get == WalletApi.FailedToUpdateWallet(MockWalletKeyApi.MainKeyVaultSaveFailure)
        val validLoad = loadedWallet.isRight
        validUpdate && validLoad
      },
      true
    )
  }

  test("importWallet: import using mnemonic from createNewWallet > Same Main Key") {
    val oldPassword = "old-password".getBytes
    val newPassword = "new-password".getBytes
    val signingInstance: ExtendedEd25519 = new ExtendedEd25519
    assertIO(
      for {
        wallet <- walletApi.createNewWallet(oldPassword).map(_.toOption.get)
        mnemonic = wallet.mnemonic
        mainKey         <- walletApi.extractMainKey(wallet.mainKeyVaultStore, oldPassword).map(_.toOption.get)
        importedWallet  <- walletApi.importWallet(mnemonic, newPassword)
        importedMainKey <- walletApi.extractMainKey(importedWallet.toOption.get, newPassword)
      } yield {
        // Should be different due to password
        val validImportedWallet = (importedWallet.isRight) && (wallet.mainKeyVaultStore != importedWallet.toOption.get)
        val testMainKey = importedMainKey.toOption.get
        // Verify the main key is the same
        val validMainKey = (importedMainKey.isRight) && (mainKey == testMainKey)
        val signature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(mainKey).signingKey, testMsg)
        val testSignature = signingInstance.sign(WalletApi.pbKeyPairToCryotoKeyPair(testMainKey).signingKey, testMsg)
        val validSignature = (java.util.Arrays.equals(signature, testSignature)) && signingInstance.verify(
          signature,
          testMsg,
          WalletApi.pbKeyPairToCryotoKeyPair(testMainKey).verificationKey
        ) && signingInstance.verify(testSignature, testMsg, WalletApi.pbKeyPairToCryotoKeyPair(mainKey).verificationKey)
        validImportedWallet && validMainKey && validSignature
      },
      true
    )

  }

  test("importWallet: an invalid mnemonic produces correct error") {
    val password = "password".getBytes
    assertIO(
      for {
        wallet <- walletApi.createNewWallet(password).map(_.toOption.get)
        mnemonic = wallet.mnemonic :+ "extraWord"
        importedWallet <- walletApi.importWallet(mnemonic, password)
      } yield importedWallet.isLeft && importedWallet.left.toOption.get == WalletApi.FailedToInitializeWallet(
        PhraseToEntropyFailure(InvalidWordLength)
      ),
      true
    )
  }

  test("importWalletAndSave: verify a save failure returns the correct error") {
    val password = "password".getBytes
    assertIO(
      for {
        wallet         <- walletApi.createNewWallet(password).map(_.toOption.get)
        importedWallet <- walletApi.importWalletAndSave[F](wallet.mnemonic, password, name = "error")
      } yield importedWallet.isLeft && (importedWallet.left.toOption.get == WalletApi.FailedToSaveWallet(
        MockWalletKeyApi.MainKeyVaultSaveFailure
      )),
      true
    )
  }

  test("saveMnemonic: verify a simple save") {
    val name = "test"
    assertIO(
      for {
        res <- walletApi.saveMnemonic(IndexedSeq("a", "b", "c"), name)
      } yield res.isRight && MockWalletKeyApi.mnemonicInstance.contains(name),
      true
    )

  }
}
