---
sidebar_position: 3
title: Mint Custom Asset
description: Mint custom Asset tokens from scratch
---

## Use Case

To mint a new custom asset.

**Objectives:**
- Mint Group Constructor tokens
- Mint Series Constructor tokens
- Mint Asset Tokens

## Set-Up

To follow along with this tutorial, you will need to initialize and fund a wallet with some LVLs. 

If you were following the instructions in the [Load Wallet with Funds](./obtain-funds) tutorial, the code to set up the 
Senders wallet should look something like:

```scala title="Initializing a Wallet and Funding it with LVLs"
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.Context
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.{BifrostQueryAlgebra, GenusQueryAlgebra, RpcChannelResource}
import co.topl.brambl.models.{Datum, Event}
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.syntax.{LvlType, valueToQuantitySyntaxOps, valueToTypeIdentifierSyntaxOps, int128AsBigInt}
import co.topl.brambl.wallet.{CredentiallerInterpreter, WalletApi}

import java.io.File
import java.nio.file.Paths

implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

// Replace with the desired location for your tutorial directory
val tutorialDir = Paths.get(System.getProperty("user.home"), "tutorial").toString
new File(tutorialDir).mkdirs() // Create the directory if it doesn't exist

def initFilePath(fileName: String): String = {
  val filePath = Paths.get(tutorialDir, fileName).toString
  new File(filePath).delete() // Clear the file if it already exists
  filePath
}

// Replace with the desired location for your key file
val keyFile = initFilePath("keyfile.json")
// Replace with the desired location of for your mnemonic file
val mnemonicFile = initFilePath("mnemonic.txt")
// Replace with the desired location of for your wallet state DB file
val walletDb = initFilePath("wallet.db")

val walletKeyApi = WalletKeyApi.make[IO]()
val walletApi = WalletApi.make(walletKeyApi)
val conn = WalletStateResource.walletResource(walletDb)
val walletStateApi = WalletStateApi.make[IO](conn, walletApi)

val initializeWallet = for {
  walletResult <- walletApi.createAndSaveNewWallet[IO]("password".getBytes, name = keyFile, mnemonicName = mnemonicFile)
  mainKeyPair <- walletApi.extractMainKey(walletResult.toOption.get.mainKeyVaultStore, "password".getBytes())
  childKeyPair <- walletApi.deriveChildKeysPartial(mainKeyPair.toOption.get, 1, 1)
  _ <- walletStateApi.initWalletState(PRIVATE_NETWORK_ID, MAIN_LEDGER_ID, childKeyPair.vk)
} yield mainKeyPair

// Replace with the address and port of your node's gRPC endpoint
val channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)
val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource)
val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)

val unprovenTransaction = for {
  _ <- initializeWallet
  heightLock <- walletStateApi.getLock("nofellowship", "genesis", 1)
  heightAddress <- txBuilder.lockAddress(heightLock.get)
  txos <- genusQueryApi.queryUtxo(heightAddress)
  sigLock <- walletStateApi.getLock("self", "default", 1)
  sigAddress <- txBuilder.lockAddress(sigLock.get)
  tx <- txBuilder.buildTransferAmountTransaction(
    LvlType,
    txos,
    heightLock.get.getPredicate,
    100L,
    sigAddress,
    heightAddress,
    1L
  )
} yield tx.toOption.get

val proveAndValidateResult = for {
  tx <- unprovenTransaction
  mainKey <- walletApi.loadAndExtractMainKey[IO]("password".getBytes, keyFile)
  credentialler = CredentiallerInterpreter.make[IO](walletApi, walletStateApi, mainKey.toOption.get)
  ctx = Context[IO](tx, 50, Map("header" -> Datum().withHeader(Datum.Header(Event.Header(50)))).lift)
  res <- credentialler.proveAndValidate(tx, ctx)
} yield res

val bifrostQuery = BifrostQueryAlgebra.make[IO](channelResource)

val broadcastTransaction = for {
  provenTx <- proveAndValidateResult
  txId <- bifrostQuery.broadcastTransaction(provenTx.toOption.get)
} yield txId

broadcastTransaction.unsafeRunSync()

// Allow some time to pass before querying the transaction
Thread.sleep(15000)

// optionally view your funds
val queryFunds = for {
  sigLock <- walletStateApi.getLock("self", "default", 1)
  sigAddress <- txBuilder.lockAddress(sigLock.get)
  txos <- genusQueryApi.queryUtxo(sigAddress)
} yield txos.map(_.transactionOutput.value.value).map(value => s"${value.typeIdentifier}: ${value.quantity.intValue}")

queryFunds.unsafeRunSync().foreach(println)
```

After running that file, you should see that the Sender's wallet has been initialized with 100 LVLs:

```bash title="output"
LvlType: 100
```

Keep the same local node instance running. We will be using it for the rest of this tutorial.

## Step 1: Mint Group Constructor Tokens 

TBD

### TBD

TBD

### Putting it all together

Your code should look something like this:

```scala title="Mint Group Constructor Tokens"

```

Try running the code. If all went well, you should see that you have 99 LVLs and 5 Group Constructor Token in your wallet,
ready to be spent by you.

```bash title="output"

```

## Step 2: Mint Series Constructor Tokens

TBD

### TBD

TBD

### Putting it all together

Your code should look something like this:

```scala title="Mint Series Constructor Tokens"

```

After ensuring that some time has passed since minting the Group Constructor Tokens from Step 1 (15-20 seconds), try 
running the code. If all went well, you should see that you have 98 LVLs, 5 Group Constructor Token, and 5 Series 
Constructor token in your wallet, ready to be spent by you.

```bash title="output"

```

## Step 3: Mint Asset Tokens

TBD

### TBD

TBD

### Putting it all together

At this point, your code should look something like this:

```scala title="Mint Asset Tokens"

```

After ensuring that some time has passed since minting the Series Constructor Tokens from Step 2 (15-20 seconds), try
running the code. If all went well, you should see that you have 97 LVLs, 5 Group Constructor Token, 5 Series Constructor
Token, and 10 Asset token in your wallet, ready to be spent by you.

```bash title="output"

```

:::note
The above output will differ if you configured your Series Policy differently.
:::

For example, if you configured your Series Policy's `tokenSupply` to be 5, you should see that you have 3 Series 
Constructor Tokens remaining instead of 5. This is because setting the `tokenSupply` field indicates that the Series 
Constructor Tokens have a limited supply. In other words, a Series Constructor Token is burned for every 5 Asset Tokens 
minted, leaving 3 remaining as change. For more information on the possible configurations of a Series Policy, see
[TIP-0003](https://github.com/Topl/tips/tree/main/TIP-0003).