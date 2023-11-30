---
sidebar_position: 1
title: Load Wallet with Funds
description: Populate a wallet with LVL tokens from your Local Node
---

## Use Case

Coming Soon

Most of these tutorials require initial funds to be present in a wallet. This tutorial will show you how to load a wallet with LVL tokens from your Local Node.
The funds come from the Genesis block in you Local Node.

## Set-Up

Get started with launching a Local Node:

1. [Get Docker](https://docs.docker.com/get-docker/)
2. Pull latest node image
   ```bash
    docker pull docker.io/toplprotocol/bifrost-node:2.0.0-alpha10
   ``` 
3. Run a node
   ```bash
    docker run --rm -p 9085:9085 -p 9084:9084 -p 9091:9091 docker.io/toplprotocol/bifrost-node:2.0.0-alpha10
   ```

## Step X: Create and Initialize a Wallet

Before we can load a wallet with funds, we need to create the wallet. There are 2 steps to this process: creating the wallet's 
Topl main key and initializing the wallet state.

### Create the Wallet's Topl Main Key

> See [Create a Wallet](../../reference/wallets/create)

We will generate a new Topl Main Key for the wallet using `createAndSaveNewWallet`. This will also save the keyfile and 
mnemonic to the local file system.

1. Initialize a Wallet Key API to persist the keyfile and mnemonic. Here we are using the provided default implementation
provided by the Service Kit to persist to the local file system.
   ```scala
   val walletKeyApi = WalletKeyApi.make[IO]()
   ```
2. Using the `walletKeyApi` created above, initialize a Wallet Api
   ```scala
   val walletApi = WalletApi.make(walletKeyApi)
   ```
3. Using the `walletApi` created above, create and persist a new wallet using `createAndSaveNewWallet`
   ```scala
   walletResult <- walletApi.createAndSaveNewWallet[IO]("password".getBytes, name = "keyfile.json", mnemonicName = "mnemonic.txt")
   ```

### Initialize the Wallet State

> See [Initialize Wallet State](../../reference/wallet-state#initialize-wallet-state)


We will initialize the wallet state using `initWalletState`. Here we are using the provided default implementation 
provided by the Service Kit to persist to a SQLite database file.

1. With the `walletApi` created in the previous section, initialize a Wallet State API
   ```scala
   val walletStateApi = WalletStateApi.make[IO](WalletStateResource.walletResource("wallet.db"), walletApi)
   ```
2. Using the `walletResult` created above, extract the Topl main key.
   ```scala
   mainKeyPair <- walletApi.extractMainKey(walletResult.mainKeyVaultStore, "password".getBytes())
   ```
3. Using the `mainKeyPair` created above, derive a child key at (1, 1).
   ```scala
   childKeyPair <- walletApi.deriveChildKeysPartial(mainKeyPair, 1, 1)
   ```
4. Using the `childKeyPair` and `walletStateApi` created above, initialize the wallet state using `initWalletState`
   ```scala
   walletStateApi.initWalletState(MAIN_NETWORK_ID, MAIN_LEDGER_ID, childKeyPair.vk)
   ```

### Breakpoint Check

At this point, your code should look something like this:

```scala
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.wallet.WalletApi

import java.io.File

implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

val homeDir = System.getProperty("user.home")
// Replace with the desired location for your key file
val keyFile = new File(homeDir, "keyfile.json").getCanonicalPath
// Replace with the desired location of for your mnemonic file
val mnemonicFile = new File(homeDir, "mnemonic.txt").getCanonicalPath
// Replace with the desired location of for your wallet state DB file
val walletDb = new File(homeDir, "wallet.db").getCanonicalPath

val walletKeyApi = WalletKeyApi.make[IO]()
val walletApi = WalletApi.make(walletKeyApi)
val walletStateApi = WalletStateApi.make[IO](WalletStateResource.walletResource(walletDb), walletApi)

val initializeWallet = for {
   walletResult <- walletApi.createAndSaveNewWallet[IO]("password".getBytes, name = keyFile, mnemonicName = mnemonicFile)
   mainKeyPair <- walletApi.extractMainKey(walletResult.toOption.get.mainKeyVaultStore, "password".getBytes())
   childKeyPair <- walletApi.deriveChildKeysPartial(mainKeyPair.toOption.get, 1, 1)
   _ <- walletStateApi.initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID, childKeyPair.vk)
} yield ()

initializeWallet.unsafeRunSync()
```

Running this code would create 3 files in your home directory: `keyfile.json`, `mnemonic.txt`, and `wallet.db`.

## Step X: Create Transaction

After you have a wallet initialized, the next step is to build a transaction that will populate your wallet with some funds. 
This transaction should transfer funds from the Genesis block to your wallet.

### Query Genesis Funds
The Genesis block encumbers tokens with a height lock. To retrieve these tokens, we need to create the LockAddress for this Height Lock.
This LockAddress will also be used as the change address for the transaction (any excess funds will go back to this Address).

1. Retrieve HeightLock from the Wallet State API. Since we are using the provided default implementation of the Wallet State API,
we can retrieve the HeightLock using the following code:
   ```scala
   heightLock <- walletStateApi.getLock("nofellowship", "genesis", 0)
   ```
2. Create a LockAddress from the HeightLock retrieved in the previous step. See [Initialize Wallet State](../../reference/locks/create-lock-addr)
   ```scala
   lockAddress <- TransactionBuilderApi.make[IO](MAIN_NETWORK_ID, MAIN_LEDGER_ID).lockAddress(heightLock)
   ```
3. With the LockAddress created in the previous step, we can query the Genesis block for funds. This will return a list of UTXOs
which will be the inputs for the transaction.

### Generate a New Lock Address to Receive Tokens

The output of the transaction should be a new LockAddress that our wallet can prove ownership of. To create this LockAddress

### Step X: Update Wallet State

TBD

### Build the Transaction

TBD

### Breakpoint Check

TBD

## Step X: Prove Transaction

TBD

## Step X: Broadcast Transaction

TBD

## Optional Step: Check Balance

TBD

## Putting It All Together