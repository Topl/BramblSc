---
sidebar_position: 2
title: Transfer Tokens
description: How to transfer tokens from one wallet to another
---

## Use Case

Coming Soon (send 10 LVLs from one wallet to another). 

In this tutorial, we have 2 users, each with their own wallet. The first is the Sender, the second is the recipient.

For the purposes of this tutorial, we will assume that the Sender already has an initialized wallet with 100 LVLs.
The sender wants to send 10 LVLs to the recipient.

To illustrate setting up a wallet, the Recipient will create and initialize a new wallet.

## Set-Up
Throughout this tutorial, we will be alternating between the Sender and the Recipient's Point-of-View. To distinguish 
between the two, each section will be prefixed with either "Sender >" or "Recipient >". We will also keep all the 
generated files (key file, wallet file, etc.) in separate folders for each user.

To follow along with this tutorial, you will need to initialize and fund the Sender's wallet with 100 LVLs. 

If you were following the instructions in the [Load Wallet with Funds](./obtain-funds) tutorial, the code to set up the 
Senders wallet should look something like:

```scala title="Initializing the Sender's Wallet and Funding it with 100 LVLs"
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

// Replace with the desired location for your *Sender* tutorial directory
val tutorialDir = Paths.get(System.getProperty("user.home"), "tutorial", "sender").toString
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

## Step X: Recipient 

TBD

### Creates and Initializes a Wallet

- Create Recipients Topl main key
- Initialize the Wallet State

### Generates a New Lock Address to Receive Tokens

- generate lock
- generate lock address
- encode lock address

### Putting it all together

TBD

## Step X: Sender

TBD

### Create Transaction

- Decode Recipients lock address
- Query sender's UTXOs
- Generate new lock address for sender's change
- Save new lock address to wallet state
- Build transaction

### Prove Transaction

- Load sender's main key
- initialize credentialler
- Create context (tick and height are trivial since there is no Height Lock)
- Prove and validate transaction

### Broadcast Transaction

Link to other page "identical"

### Putting it all together

TBD

## Optional Step: Recipient > Check Balance

// insert runnable code