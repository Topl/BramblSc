---
sidebar_position: 2
title: Transfer Tokens
description: Transfer tokens from one wallet to another
---

## Use Case

To send 10 LVLs from an existing wallet to a new wallet.

**Objectives:**
- Create a new wallet and lock address (the recipient's wallet)
- Share a LockAddress with another wallet by Encoding it as a Base58 string
- Use the keys and state from an existing wallet (the sender's wallet)
- Create a transaction to 
  - transfer an amount of funds from one wallet to another
  - transfer the remaining funds back to the original wallet in a new LockAddress
- Check the balance of a LockAddress

## Set-Up

In this tutorial, we have 2 users, each with their own wallet. The first is the Sender, the second is the Recipient.

Throughout this tutorial, we will be alternating between the Sender and the Recipient's Point-of-View. To distinguish 
between the two, each section will be marked with either "Sender" or "Recipient". We will also keep all the 
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
  _ <- walletStateApi.initWalletState(PRIVATE_NETWORK_ID, MAIN_LEDGER_ID, mainKeyPair.toOption.get)
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

## Step 1: Recipient 

Before we can send tokens to the recipient, the recipient needs to have a wallet and a LockAddress that they have ownership of. 

### Create and Initialize the Recipient's Wallet

Initializing the recipient's wallet is similar to initializing the sender's wallet. For more details you can read
[Create and Initialize a Wallet](./obtain-funds#step-1-create-and-initialize-a-wallet) in the Load Wallet with Funds tutorial.

1. Create the Topl main key for the Recipient's wallet. For more details see the 
[Create the Wallet's Topl Main Key](./obtain-funds#create-the-wallets-topl-main-key)'s section in a previous tutorial.
2. Using the main key from the previous step, initialize the Recipient's Wallet State. For more details see the
[Initialize the Wallet State](./obtain-funds#initialize-the-wallet-state) section in a previous tutorial.

### Generates a New Lock Address to Receive Tokens

After the recipient's wallet has been initialized, we can generate the LockAddress for the recipient to receive the 10 LVLs.
For this tutorial, we will generate a LockAddress for a 1-of-1 Digital Signature Lock.

1. Generate the 1-of-1 Signature lock and LockAddress for the recipient. For more details see the 
[Generate a New Lock Address to Receive Tokens](./obtain-funds#generate-a-new-lock-address-to-receive-tokens) section in
a previous tutorial.
2. This LockAddress must be shared to the Sender. To support this, the Recipient will encode their LockAddress into a Base58 format.
See [Share a LockAddress](../../reference/locks/share-lock-addr). 
 ```scala
val base58Addr = encodeAddress(lockAddress)
 ```

### Putting it all together

At this point, you should have code to create a new wallet and lock address for the recipient. The code should look similar to this:

```scala title="Initializing the Recipient's Wallet and Generating a LockAddress"
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.codecs.AddressCodecs.encodeAddress
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.wallet.WalletApi

import java.io.File
import java.nio.file.Paths

implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

// Replace with the desired location for your *Recipient* tutorial directory
val tutorialDir = Paths.get(System.getProperty("user.home"), "tutorial", "recipient").toString
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
  _ <- walletStateApi.initWalletState(PRIVATE_NETWORK_ID, MAIN_LEDGER_ID, mainKeyPair.toOption.get)
  sigLock <- walletStateApi.getLock("self", "default", 1)
  lockAddress <- TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID).lockAddress(sigLock.get)
} yield encodeAddress(lockAddress)

initializeWallet.unsafeRunSync()
```

After running that file, you should see the Recipient's LockAddress:

```bash title="output"
val res1: String = ptetP7jshHVVBsmyLd5ugb9mVSehmtFdZHYQZqG3zaFtWSfiX79kYt9obJea
```

It is important to note that your LockAddress will be different from the one shown above. Everytime the code is run, a new
Topl main key is generated, thus resulting in a new LockAddress. 

## Step 2: Sender

Once the Sender has the Recipient's LockAddress, they can send begin the process of sending the 10 LVLs to the Recipient.

### Create Transaction

The Sender must build an unproven transaction to send 10 LVLs to the Recipient's LockAddress. Any change (excess LVLs) from the 
transaction should go to a new LockAddress that the Sender owns. For this tutorial, we will generate a LockAddress for a 1-of-1
Digital Signature Lock for the change.

1. Building a transaction requires specifying a LockAddress of the recipient. The Recipient shared their address as a base58 encoded
 string. The first step is to decode this lock address. See [Share a LockAddress](../../reference/locks/share-lock-addr).
  ```scala
  val recipientAddr = decodeAddress("ptetP7jshHVVBsmyLd5ugb9mVSehmtFdZHYQZqG3zaFtWSfiX79kYt9obJea")
  ```
2. From the Sender's initialization (in the Set-Up section), the Sender loaded funds to their wallet, encumbered by a 1-of-1 Signature Lock
stored at (fellowship="self", template="default", nextInteraction=1). To obtain the funds, we must query Genus for the UTXOs. This is similar
to the section [Query Genesis Funds](./obtain-funds#query-genesis-funds) in the Obtain Funds tutorial, however, the parameters for
`getLock` in the first step is different.
  ```scala
  inputLock <- walletStateApi.getLock("self", "default", 1)
  ```
3. Since the Sender owns 100 LVLs but is only sending 10 to the Recipient, we must create a new LockAddress for the change. 
For this tutorial, we will generate a LockAddress for a 1-of-1 Digital Signature Lock for the change. This is very similar
to the [Generate a New Lock Address to Receive Tokens](./obtain-funds#generate-a-new-lock-address-to-receive-tokens) section in
a previous tutorial, however, we are using `nextInteraction=2` for step 1 (to prevent address re-use).
  ```scala
changeLock <- walletStateApi.getLock("self", "default", 2)
  ```
4. Since we are creating a new LockAddress for the change (that we want the Sender to be able to spend from in the future),
we must update the Wallet State with information to unlock the lock. We did not have to do this step in the set-up or previous
sections since Wallet State Initialization populates the information for the initial Signature Lock at indices (1, 1, 1). 
  ```scala
  walletStateApi.updateWalletState(
    changePredicate, // The predicate contained in changeLock
    changeAddress, // The LockAddress of the changeLock
    Some("ExtendedEd25519"), // The signing routine of the Signature Lock
    changeVk, // The verification key of the Signature Lock. This is the same as deriving the main key with indices (1, 1, 2)
    Indices(1,1,2) // The indices of the Signature Lock
  )
  ```
5. Using everything we generated in this section, we can finally build the transaction using the Transaction Builder API. This mirrors
the [Build the Transaction](./obtain-funds#build-the-transaction) section in the Obtain Funds tutorial.
  ```scala
TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID).buildTransferAmountTransaction(
   LvlType, // We are transferring LVLs
   txos,  // The UTXOs we queried from step 2
   inputLock.getPredicate, // The existing lock we retrieved as part of step 2 
   10L, // The amount of LVLs we want to transfer to the recipient
   recipientAddr, // The decoded LockAddress of the recipient from step 1
   changeAddress, // The LockAddress of the changeLock from step 3
   1L // An arbitrary fee amount
)
  ```

### Prove Transaction

Once we have built the unproven transaction, we must prove (and then validate) it. This is similar to the 
[Prove Transaction](./obtain-funds#step-3-prove-transaction) section in the Obtain Funds tutorial, with a couple differences.

We must load the main key from the Sender's existing wallet to intialize the Credentialler with. 
See [Load and Decrypt a Topl Main Key Pair](../../reference/wallets/usage#load-and-decrypt-a-topl-main-key-pair).

```scala
mainKey <- walletApi.loadAndExtractMainKey[IO]("password".getBytes, keyFile)
```

We can also simplify the Context used from step 2 in the Obtain Funds tutorial since we are not using a Height Lock:

```scala
ctx = Context[IO](unprovenTransaction, 50, _ => None)
```

### Broadcast Transaction

Once the transaction is proven and validated, the Sender can broadcast the transaction to the network. This is exactly 
identical to the [Broadcast Transaction](./obtain-funds#step-4-broadcast-transaction) section in the Obtain Funds tutorial.

### Putting it all together

At this point, you should have code to create and submit a transaction to send 10 LVLs from the Sender's wallet to the 
Recipient's LockAddress. The code should look similar to this:

```scala title="Sender submits TX to send 10 LVLs to Recipient"
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.Context
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.codecs.AddressCodecs.decodeAddress
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.{BifrostQueryAlgebra, GenusQueryAlgebra, RpcChannelResource}
import co.topl.brambl.models.Indices
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.syntax.LvlType
import co.topl.brambl.utils.Encoding
import co.topl.brambl.wallet.{CredentiallerInterpreter, WalletApi}
import quivr.models.VerificationKey

import java.nio.file.Paths

implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

// Replace with the existing location for your *Sender* tutorial directory
val tutorialDir = Paths.get(System.getProperty("user.home"), "tutorial", "sender").toString
// Replace with the existing location for your key file
val keyFile = Paths.get(tutorialDir, "keyfile.json").toString
// Replace with the existing location of for your wallet state DB file
val walletDb = Paths.get(tutorialDir, "wallet.db").toString
val conn = WalletStateResource.walletResource(walletDb)

// Replace with the address and port of your node's gRPC endpoint
val channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)

val walletKeyApi = WalletKeyApi.make[IO]()
val walletApi = WalletApi.make(walletKeyApi)
val walletStateApi = WalletStateApi.make[IO](conn, walletApi)
val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource)
val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)

// Replace the string with your Recipient's Base58 lock address
val recipientAddr = decodeAddress("ptetP7jshHVVBsmyLd5ugb9mVSehmtFdZHYQZqG3zaFtWSfiX79kYt9obJea").toOption.get

val unprovenTransaction = for {
  inputLock <- walletStateApi.getLock("self", "default", 1)
  inputAddress <- txBuilder.lockAddress(inputLock.get)
  txos <- genusQueryApi.queryUtxo(inputAddress)
  changeLock <- walletStateApi.getLock("self", "default", 2)
  changeAddress <- txBuilder.lockAddress(changeLock.get)
  selfDefaultVk <- walletStateApi.getEntityVks("self", "default").map(_.get.head) map { vk =>
    VerificationKey.parseFrom(
      Encoding.decodeFromBase58(vk).toOption.get
    )
  }
  changeVk <- walletApi.deriveChildVerificationKey(selfDefaultVk, 2)
  _ <- walletStateApi.updateWalletState(
    Encoding.encodeToBase58(changeLock.get.getPredicate.toByteArray),
    changeAddress.toBase58(),
    Some("ExtendedEd25519"),
    Some(Encoding.encodeToBase58(changeVk.toByteArray)),
    Indices(1, 1, 2)
  )
  tx <- txBuilder.buildTransferAmountTransaction(
    LvlType,
    txos,
    inputLock.get.getPredicate,
    10L,
    recipientAddr,
    changeAddress,
    1L
  )
} yield tx.toOption.get

val provenTx = for {
  mainKey <- walletApi.loadAndExtractMainKey[IO]("password".getBytes, keyFile)
  credentialler = CredentiallerInterpreter.make[IO](walletApi, walletStateApi, mainKey.toOption.get)
  tx <- unprovenTransaction
  ctx = Context[IO](tx, 50, _ => None)
  res <- credentialler.proveAndValidate(tx, ctx)
} yield res.toOption.get


val bifrostQuery = BifrostQueryAlgebra.make[IO](channelResource)

val broadcastTransaction = for {
  tx <- provenTx
  _ <- bifrostQuery.broadcastTransaction(tx)
} yield "Transaction Submitted"

broadcastTransaction.unsafeRunSync()
```

After running that file, you should see a message that the transaction was submitted to the network. 

```bash title="output"
val res0: String = Transaction Submitted
```

## Optional Steps: Check Balances

After waiting a short period of time, the Recipient and Sender can check their balances to see if the transaction was 
processed successfully. 

### Recipient Checks Balance

The following code shows an example of the recipient checking their balance:

```scala title="Recipient checks balance"
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.{GenusQueryAlgebra, RpcChannelResource}
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.syntax.{int128AsBigInt, valueToQuantitySyntaxOps, valueToTypeIdentifierSyntaxOps}
import co.topl.brambl.wallet.WalletApi

import java.nio.file.Paths

implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

// Replace with the existing location for your *Recipient* tutorial directory
val tutorialDir = Paths.get(System.getProperty("user.home"), "tutorial", "recipient").toString
// Replace with the existing location of for your wallet state DB file
val walletDb = Paths.get(tutorialDir, "wallet.db").toString
val conn = WalletStateResource.walletResource(walletDb)

// Replace with the address and port of your node's gRPC endpoint
val channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)

val walletKeyApi = WalletKeyApi.make[IO]()
val walletApi = WalletApi.make(walletKeyApi)
val walletStateApi = WalletStateApi.make[IO](conn, walletApi)
val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource)
val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)

val queryFunds = for {
  // The sender sent funds to the address derived from (self, default, 1)
  sigLock <- walletStateApi.getLock("self", "default", 1)
  sigAddress <- txBuilder.lockAddress(sigLock.get)
  txos <- genusQueryApi.queryUtxo(sigAddress)
} yield txos.map(_.transactionOutput.value.value).map(value => s"${value.typeIdentifier}: ${value.quantity.intValue}")

queryFunds.unsafeRunSync().foreach(println)
```

If all goes well, the recipient should see a balance of 10 LVLs.

```bash title="output"
LvlType: 10
```

### Sender Checks Balance

The following code shows an example of the sender checking their balance:

```scala title="Sender checks balance"
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.{GenusQueryAlgebra, RpcChannelResource}
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.syntax.{int128AsBigInt, valueToQuantitySyntaxOps, valueToTypeIdentifierSyntaxOps}
import co.topl.brambl.wallet.WalletApi

import java.nio.file.Paths

implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

// Replace with the existing location for your *Sender* tutorial directory
val tutorialDir = Paths.get(System.getProperty("user.home"), "tutorial", "sender").toString
// Replace with the existing location of for your wallet state DB file
val walletDb = Paths.get(tutorialDir, "wallet.db").toString
val conn = WalletStateResource.walletResource(walletDb)

// Replace with the address and port of your node's gRPC endpoint
val channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)

val walletKeyApi = WalletKeyApi.make[IO]()
val walletApi = WalletApi.make(walletKeyApi)
val walletStateApi = WalletStateApi.make[IO](conn, walletApi)
val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource)
val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)

val queryFunds = for {
  // The change went to the address derived from (self, default, 2)
  sigLock <- walletStateApi.getLock("self", "default", 2)
  sigAddress <- txBuilder.lockAddress(sigLock.get)
  txos <- genusQueryApi.queryUtxo(sigAddress)
} yield txos.map(_.transactionOutput.value.value).map(value => s"${value.typeIdentifier}: ${value.quantity.intValue}")

queryFunds.unsafeRunSync().foreach(println)
```

If all goes well, the sender should see a balance of 89 LVLs in their change address:

```bash title="output"
LvlType: 89
```

The 89 LVLs is the result of the 100 LVLs the sender had before the transaction minus the 10 LVLs they sent to the recipient
minus the 1 LVL fee they paid to the network.