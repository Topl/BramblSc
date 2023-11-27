---
sidebar_position: 3
---

# Usage of the Service Kit

In this document we are showing how to use the service kit to interact with the
Topl Network. We will be showing code snippets in Scala.

## Creating a Vault and a Wallet Database

The first step is to create a vault. The vault is where the master key is stored.

```scala
// You can run this code using scala-cli. Save it in a file called `create-vault.sc` and run it with `scala-cli create-vault.sc`
//> using scala 2.13
//> using repository "sonatype-s01:releases"
//> using dep "co.topl::service-kit:2.0.0-beta0"
//> using dep "org.typelevel::cats-core:2.10.0"

import cats.effect.IO
import co.topl.brambl.wallet.WalletApi
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.constants.NetworkConstants

import cats.effect.std
import io.circe.syntax._
import co.topl.crypto.encryption.VaultStore.Codecs._
import cats.effect.unsafe.implicits.global

import java.io.File

case class CreateWallet(file: String, password: String) {
  val walletKeyApi = WalletKeyApi.make[IO]()
  val walletApi = WalletApi.make(walletKeyApi)
  val walletStateApi = WalletStateApi.make[IO](WalletStateResource.walletResource(file), walletApi)

  val createWallet = for {
    wallet <- walletApi
      // highlight-start
      .createNewWallet(
        password.getBytes(),
        Some("passphrase")
      )
      // highlight-end
      .map(_.fold(throw _, identity))
    keyPair <- walletApi
      // highlight-start
      .extractMainKey(
        wallet.mainKeyVaultStore,
        password.getBytes()
      )
      // highlight-end
      .flatMap(
        _.fold(
          _ =>
            IO.raiseError(
              new Throwable("No input file (should not happen)")
            ),
          IO(_)
        )
      )
    _ <- std.Console[IO].println("Wallet: " + new String(wallet.mainKeyVaultStore.asJson.noSpaces))
    _ <- std.Console[IO].println("Mnemonic: "+ wallet.mnemonic.mkString(","))
    // highlight-next-line
    derivedKey <- walletApi.deriveChildKeysPartial(keyPair, 1, 1)
    // Initialize the wallet state:
    // highlight-start
    _ <- walletStateApi.initWalletState(
      NetworkConstants.PRIVATE_NETWORK_ID,
      NetworkConstants.MAIN_LEDGER_ID,
      derivedKey.vk
    )
    // highlight-end
  } yield ()

}

val file = "myWallet.db"
val password = "password"
// we delete the wallet before creating it
new File(file).delete()

val wallet = CreateWallet(file, password)
// Create the wallet using:
// highlight-next-line
wallet.createWallet.unsafeRunSync()
```

This will create an encrypted vault and print it to the console. The vault is
encrypted with the password provided. It will also create a wallet state database
file called `myWallet.db` and print the mnemonic to recover the wallet to the
standard ouput.

This code has several parts:

- first, it creates a wallet in memory (the `walletApi.createNewWallet` function)
- then, it extracts the main key from the wallet (the `walletApi.extractMainKey` function)
- then, it derives a child key from the main key (the `walletApi.deriveChildKeysPartial` function),
this is needed to initialized the wallet database with an initial entry for the "self" and "default" fellowship and template.
- finally, it initializes the wallet database (the `walletStateApi.initWalletState` function)

## Updating the Wallet Database

Users must update the wallet state whenever one of their child keys is used to create a new transaction.

```scala
// You can run this code using scala-cli. Save it in a file called `create-vault.sc` and run it with `scala-cli create-vault.sc`
//> using scala 2.13
//> using repository "sonatype-s01:releases"
//> using dep "co.topl::service-kit:2.0.0-beta0"
//> using dep "org.typelevel::cats-core:2.10.0"

import cats.effect.IO
import co.topl.brambl.wallet.WalletApi
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.constants.NetworkConstants
import cats.effect.std
import io.circe.syntax._
import co.topl.crypto.encryption.VaultStore.Codecs._
import cats.effect.unsafe.implicits.global
import cats.implicits.toTraverseOps
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.models.Indices
import co.topl.brambl.utils.Encoding
import quivr.models.VerificationKey

import java.io.File

case class CreateWallet(file: String, password: String) {
  val walletKeyApi = WalletKeyApi.make[IO]()
  val walletApi = WalletApi.make(walletKeyApi)
  val walletStateApi = WalletStateApi.make[IO](WalletStateResource.walletResource(file), walletApi)

  val createWallet = for {
    wallet <- walletApi
      .createNewWallet(
        password.getBytes(),
        Some("passphrase")
      )
      .map(_.fold(throw _, identity))
    keyPair <- walletApi
      .extractMainKey(
        wallet.mainKeyVaultStore,
        password.getBytes()
      )
      .flatMap(
        _.fold(
          _ =>
            IO.raiseError(
              new Throwable("No input file (should not happen)")
            ),
          IO(_)
        )
      )
    _ <- std.Console[IO].println("Wallet: " + new String(wallet.mainKeyVaultStore.asJson.noSpaces))
    _ <- std.Console[IO].println("Mnemonic: "+ wallet.mnemonic.mkString(","))
    derivedKey <- walletApi.deriveChildKeysPartial(keyPair, 1, 1)
    // Initialize the wallet state:
    _ <- walletStateApi.initWalletState(
      NetworkConstants.PRIVATE_NETWORK_ID,
      NetworkConstants.MAIN_LEDGER_ID,
      derivedKey.vk
    )
  } yield ()

  // highlight-start
  val updateWallet = for {
    indices <- IO.pure(Indices(1, 1, 2))
    lock <- walletStateApi.getLock("self", "default", indices.z).map(_.get)
    lockAddress <- TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID).lockAddress(lock).map(_.toBase58())
    lockPredicate = Encoding.encodeToBase58Check(lock.getPredicate.toByteArray)
    parentVk <- walletStateApi.getEntityVks("self", "default")
      .map(_.sequence.head.map(pVk => VerificationKey.parseFrom(Encoding.decodeFromBase58(pVk).toOption.get)))
    vk <- parentVk.map(pVk => walletApi.deriveChildVerificationKey(pVk, indices.z)
      .map(cVk => Encoding.encodeToBase58(cVk.toByteArray))).sequence
    _ <- walletStateApi.updateWalletState(lockPredicate, lockAddress, Some("ExtendedEd25519"), vk, indices)
  } yield ()
  // highlight-end
}

val file = "myWallet.db"
val password = "password"
// we delete the wallet before creating it
new File(file).delete()

val wallet = CreateWallet(file, password)
wallet.createWallet.unsafeRunSync()

// Update the wallet using:
// highlight-next-line
wallet.updateWallet.unsafeRunSync()
```

This builds off of the example from the previous section. A new part is added to update the wallet state.

This addition has several parts:
- first, it sets the indices to use for the update. Here we chose (x=1, y=1, z=2). Per the `initWalletState`, (x=1, y=1) represents
the "self" fellowship and the "default" template.
- then, it creates the lock for these indices. We use the `getLock` function to retrieve the lock from the wallet state 
for `self`, `default` and z=2 (per the indices chosen). We know this fellowship and default template exist because it was 
initialized per `initWalletState`.
- then, it creates the lock address from the lock. We used the TransactionBuilder to accomplish this, however, you can 
build one manually if you desire.
- then, it retrieves the predicate from the lock and encodes it to base58.
- then, it retrieves the parent verification key from the wallet existing state using `getEntityVks` for the "self" 
fellowship and the "default" template. We know this fellowship and default template exist because it was
initialized per `initWalletState`. 
- then, it derives the child verification key from the parent verification key using the `deriveChildVerificationKey` for 
the new z=2 index.
- finally, it updates the wallet state using the `updateWalletState` function.