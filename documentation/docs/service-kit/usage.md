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
//> using repository sonatype-s01:releases
//> using dep co.topl::service-kit:2.0.0-alpha7
//> using dep org.typelevel::cats-core:2.10.0

import cats.effect.kernel.Resource
import cats.effect.IO
import co.topl.brambl.wallet.WalletApi
import co.topl.brambl.servicekit.WalletStateApi
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.servicekit.WalletKeyApi
import java.sql.{Connection, DriverManager}
import cats.effect.std
import io.circe.syntax._
import co.topl.crypto.encryption.VaultStore.Codecs._
import cats.effect.unsafe.implicits.global
import java.io.File

case class CreateWallet(
  file: String,
  password: String
) {
  def walletResource(name: String): Resource[IO, Connection] = Resource
    .make(
      IO.delay(
        DriverManager.getConnection(
          s"jdbc:sqlite:${name}"
        )
      )
    )(conn => IO.delay(conn.close()))
  val walletKeyApi = WalletKeyApi.make[IO]()
  val walletApi = WalletApi.make(walletKeyApi)
  
  val walletStateApi = WalletStateApi
    .make[IO](
      walletResource(file),
      walletApi
    )
  
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
    _ <- walletStateApi.initWalletState(
      NetworkConstants.PRIVATE_NETWORK_ID,
      NetworkConstants.MAIN_LEDGER_ID,
      derivedKey.vk
    )
  } yield ()

}

val file = "myWallet.db"
val password = "password"
// we delete the wallet before creating it
new File(file).delete()

// run using:
CreateWallet(file, password).createWallet.unsafeRunSync()
```

This will create an encrypted vault and print it to the console. The vault is
encrypted with the password provided. It will also create a wallet state database
file called `myWallet.db` and print the mnemonic to recover the wallet to the
standard ouput.

This code has several parts:

- first, it creates a wallet in memory (the `walletApi.createNewWallet` function)
- then, it extracts the main key from the wallet (the `walletApi.extractMainKey` function)
- then, it derives a child key from the main key (the `walletApi.deriveChildKeysPartial` function),
this is needed to initialized the wallet database
- finally, it initializes the wallet database (the `walletStateApi.initWalletState` function)