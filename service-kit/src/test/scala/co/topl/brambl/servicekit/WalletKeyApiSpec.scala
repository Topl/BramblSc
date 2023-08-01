package co.topl.brambl.servicekit

import munit.CatsEffectSuite

import java.nio.file.Paths

class WalletKeyApiSpec extends CatsEffectSuite with BaseSpec {

  private def getFileName(name: String) = s"$TEST_DIR/$name"

  testDirectory.test("Save and get VaultStore") { _ =>
    assertIO(
      for {
        vs     <- walletApi.buildMainKeyVaultStore("dummyKeyPair".getBytes, "password".getBytes)
        saved  <- walletKeyApi.saveMainKeyVaultStore(vs, getFileName("key.json"))
        loaded <- walletKeyApi.getMainKeyVaultStore(getFileName("key.json"))
      } yield loaded.toOption.get == vs,
      true
    )
  }

  testDirectory.test("Get VaultStore > Does not exist") { _ =>
    assertIO(
      for {
        loaded <- walletKeyApi.getMainKeyVaultStore(getFileName("key.json"))
      } yield loaded.isLeft && !Paths.get(getFileName("key.json")).toFile.exists(),
      true
    )
  }

  testDirectory.test("Save, delete and get VaultStore > Does not exist") { _ =>
    assertIO(
      for {
        vs      <- walletApi.buildMainKeyVaultStore("dummyKeyPair".getBytes, "password".getBytes)
        saved   <- walletKeyApi.saveMainKeyVaultStore(vs, getFileName("key.json"))
        deleted <- walletKeyApi.deleteMainKeyVaultStore(getFileName("key.json"))
        loaded  <- walletKeyApi.getMainKeyVaultStore(getFileName("key.json"))
      } yield loaded.isLeft && !Paths.get(getFileName("key.json")).toFile.exists(),
      true
    )
  }

  testDirectory.test("Delete VaultStore > VaultStore does not exists") { _ =>
    assertIO(
      for {
        deleted <- walletKeyApi.deleteMainKeyVaultStore(getFileName("key.json"))
      } yield deleted.isLeft && !Paths.get(getFileName("key.json")).toFile.exists(),
      true
    )
  }

  testDirectory.test("Save, update, and get VaultStore") { _ =>
    assertIO(
      for {
        vs      <- walletApi.buildMainKeyVaultStore("dummyKeyPair".getBytes, "password".getBytes)
        saved   <- walletKeyApi.saveMainKeyVaultStore(vs, getFileName("key.json"))
        newVs   <- walletApi.buildMainKeyVaultStore("a different dummyKeyPair".getBytes, "password".getBytes)
        updated <- walletKeyApi.updateMainKeyVaultStore(newVs, getFileName("key.json"))
        loaded  <- walletKeyApi.getMainKeyVaultStore(getFileName("key.json"))
      } yield loaded.toOption.get == newVs,
      true
    )
  }

  testDirectory.test("Update VaultStore > VaultStore does not exists") { _ =>
    assertIO(
      for {
        vs      <- walletApi.buildMainKeyVaultStore("dummyKeyPair".getBytes, "password".getBytes)
        updated <- walletKeyApi.updateMainKeyVaultStore(vs, getFileName("key.json"))
      } yield updated.isLeft && !Paths.get(getFileName("key.json")).toFile.exists(),
      true
    )
  }

  testDirectory.test("Save Mnemonic") { _ =>
    assertIO(
      for {
        saved <- walletKeyApi.saveMnemonic(IndexedSeq("a", "b", "c"), getFileName("mnemonic.txt"))
      } yield saved.isRight && Paths.get(getFileName("mnemonic.txt")).toFile.exists(),
      true
    )
  }

  testDirectory.test("Save Mnemonic > Mnemonic already exists") { _ =>
    assertIO(
      for {
        saved      <- walletKeyApi.saveMnemonic(IndexedSeq("a", "b", "c"), getFileName("mnemonic.txt"))
        savedTwice <- walletKeyApi.saveMnemonic(IndexedSeq("a", "b", "c"), getFileName("mnemonic.txt"))
      } yield savedTwice.isLeft && Paths.get(getFileName("mnemonic.txt")).toFile.exists(),
      true
    )
  }
}
