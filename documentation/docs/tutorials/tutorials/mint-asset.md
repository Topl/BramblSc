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

Before we can mint a new Asset Token, we must first own Group Constructor Tokens. These Constructor Tokens will be
used to mint Asset Tokens. Group Constructor Tokens are minted for a given Group Policy.

### Query Genus Funds

From wallet initialization (in the Set-Up section), our wallet has 100 LVLs encumbered by a 1-of-1 Signature Lock stored 
at (fellowship="self", template="default", nextInteraction=1). To obtain the funds, we must query Genus for the UTXOs. This is similar
to the section [Query Genesis Funds](./obtain-funds#query-genesis-funds) in the Obtain Funds tutorial, however, the parameters for
`getLock` in the first step is different.

```scala
inputLock <- walletStateApi.getLock("self", "default", 1)
inputAddr <- transactionBuilderApi.lockAddress(inputLock)
txos <- genusQueryApi.queryUtxo(inputAddr)
```

### Create Group Policy

The Group Policy will be used to mint Group Constructor Tokens. The Group Policy must be associated to an existing 
UTXO that contains LVLs. Most commonly, this will be the UTXO that contains the LVLs for the transaction fee. In this 
tutorial, we will use the UTXO that contains the 100 LVLs. To see all fields of a Group Policy,
see [TIP-0003](https://github.com/Topl/tips/tree/main/TIP-0003#group-policy).

```scala 
GroupPolicy("Group Policy Label", txos.head.outputAddress)
```

### Generate a New Lock Address

Whenever we create a new transaction, we must generate new lock addresses to receive the funds and the change. 
For simplicity in this tutorial, we will use the same lock address for both the minted group constructor tokens and the change.

1. For this tutorial, we will generate a LockAddress for a 1-of-1 Digital Signature Lock for the Transaction Outputs. This is 
identical to Step 3 under [Create Transaction](./simple-transfer#create-transaction) in the Transfer Tokens tutorial.
  ```scala
  outputLock <- walletStateApi.getLock("self", "default", 2)
  ```
2. Since we are creating a new LockAddress, we must update the Wallet State with information to unlock the lock. We did 
not have to do this step in the Set-Up since Wallet State Initialization populates the information for the initial Signature 
Lock at indices (1, 1, 1). This is identical to Step 4 under [Create Transaction](./simple-transfer#create-transaction) 
in the Transfer Tokens tutorial.
  ```scala
  walletStateApi.updateWalletState(
    outputPredicate, // The predicate contained in outputLock
    outputAddress, // The LockAddress of the outputLock
    Some("ExtendedEd25519"), // The signing routine of the Signature Lock
    outputVk, // The verification key of the Signature Lock. This is the same as deriving the main key with indices (1, 1, 2)
    Indices(1,1,2) // The indices of the Signature Lock
  )
  ```

### Create Transaction

Using everything we generated in this section, we can finally build the transaction using the Transaction Builder API.
We will be using the `buildGroupMintingTransaction` function. 
Read [Mint Group Constructor Tokens](../../reference/transactions/minting#mint-group-constructor-tokens) for more
information.

```scala
TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID).buildGroupMintingTransaction(
 txos,  // The UTXOs we queried from "Query Genus Funds"
 inputLock.getPredicate, // The existing lock we retrieved from "Query Genus Funds"
 groupPolicy, // The group policy we created from "Create Group Policy"
 5L, // The amount of Group Constructor Tokens we want to mint
 mintedAddress, // The lock address we created from "Generate a New Lock Address"
 changeAddress, // The lock address we created from "Generate a New Lock Address"
 1L // An arbitrary fee amount
)
```

### Prove Transaction

Once we have built the unproven transaction, we must prove (and then validate) it. This is identical 
to [Prove Transaction](./simple-transfer#prove-transaction) from the Transfer Tokens tutorial.

### Broadcast Transaction

Once the transaction is proven and validated, the Sender can broadcast the transaction to the network. This is exactly 
identical to the [Broadcast Transaction](./obtain-funds#step-4-broadcast-transaction) section from previous tutorials.

### Putting it all together

Your code should look something like this:

```scala title="Mint Group Constructor Tokens"
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.Context
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.{BifrostQueryAlgebra, GenusQueryAlgebra, RpcChannelResource}
import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.models.Indices
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.syntax.{int128AsBigInt, valueToQuantitySyntaxOps, valueToTypeIdentifierSyntaxOps}
import co.topl.brambl.utils.Encoding
import co.topl.brambl.wallet.{CredentiallerInterpreter, WalletApi}
import quivr.models.VerificationKey

import java.nio.file.Paths

implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

// Replace with the desired location for your tutorial directory
val tutorialDir = Paths.get(System.getProperty("user.home"), "tutorial").toString
// Replace with the desired location for your key file
val keyFile = Paths.get(tutorialDir, "keyfile.json").toString
// Replace with the desired location of for your wallet state DB file
val walletDb = Paths.get(tutorialDir, "wallet.db").toString
val conn = WalletStateResource.walletResource(walletDb)
// Replace with the address and port of your node's gRPC endpoint
val channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)

val walletKeyApi = WalletKeyApi.make[IO]()
val walletApi = WalletApi.make(walletKeyApi)
val walletStateApi = WalletStateApi.make[IO](conn, walletApi)
val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource)
val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)

val unprovenTransaction = for {
  inputLock <- walletStateApi.getLock("self", "default", 1)
  inputAddress <- txBuilder.lockAddress(inputLock.get)
  txos <- genusQueryApi.queryUtxo(inputAddress)
  groupPolicy = GroupPolicy("Group Policy Label", txos.head.outputAddress)
  outputLock <- walletStateApi.getLock("self", "default", 2)
  outputAddress <- txBuilder.lockAddress(outputLock.get)
  outputVk <- walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
    // Derive the verification key at path 1/1/2 (used in outputLock)
    walletApi.deriveChildVerificationKey(VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get), 2)
  }
  _ <- walletStateApi.updateWalletState(
    Encoding.encodeToBase58(outputLock.get.getPredicate.toByteArray),
    outputAddress.toBase58(),
    Some("ExtendedEd25519"),
    Some(Encoding.encodeToBase58(outputVk.toByteArray)),
    Indices(1, 1, 2)
  )
  tx <- txBuilder.buildGroupMintingTransaction(
    txos,
    inputLock.get.getPredicate,
    groupPolicy,
    5L,
    outputAddress,
    outputAddress,
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
} yield ()

broadcastTransaction.unsafeRunSync()

val queryFunds = for {
  // The tokens were sent to the address derived from (self, default, 2)
  sigLock <- walletStateApi.getLock("self", "default", 2)
  sigAddress <- txBuilder.lockAddress(sigLock.get)
  txos <- genusQueryApi.queryUtxo(sigAddress)
} yield txos.map(_.transactionOutput.value.value).map(value =>
  s"${value.typeIdentifier.getClass.getSimpleName}: ${value.quantity.intValue}"
)

// Allow some time to pass before querying the transaction
Thread.sleep(15000)

queryFunds.unsafeRunSync().foreach(println)
```

Try running the code. If all went well, you should see that you have 99 LVLs and 5 Group Constructor Token in your wallet,
ready to be spent by you.

```bash title="output"
LvlType$: 99
GroupType: 5
```

Keep the same local node instance running.

## Step 2: Mint Series Constructor Tokens

Before we can mint a new Asset Token, we must also own Series Constructor Tokens. These Constructor Tokens will be
used to mint Asset Tokens. Series Constructor Tokens are minted for a given Series Policy. The steps to mint Series Constructor
Tokens are very similar to minting Group Constructor Tokens, consequently, this section will reference prior sections of this
tutorial where possible.

### Query Genus Funds

From the previous section, our wallet should have 99 LVLs and 5 Group Constructor Tokens. encumbered by a 1-of-1 Signature 
Lock stored at (fellowship="self", template="default", nextInteraction=2). To obtain the funds, we must query Genus for 
the UTXOs. This is very similar to the [previous section](#query-genus-funds), however, we are using `nextInteraction=2` 
instead of `nextInteraction=1`.

### Create Series Policy

We need to create the Series Policy that will be used to mint Series Constructor Tokens. This is similar to 
[creating a group policy](#create-group-policy), however, a Series Policy has different fields. To see all fields of a
Series Policy, see [TIP-0003](https://github.com/Topl/tips/tree/main/TIP-0003#series-policy).

Identical to the Group Policy, the Series Policy must be associated to an existing UTXO 
that contains LVLs (the `registrationUtxo` field). Most commonly, this will be the UTXO that contains the LVLs for the 
transaction fee. In this tutorial, we will use the UTXO that contains the 99 LVLs. Since our UTXO with 99 LVLs is encumbered 
by the same Lock Address as our UTXO with 5 Group Constructor Tokens, we will need to filter out the TXOs from the previous 
section. 

For example:

```scala
SeriesPolicy("Series Policy Label", registrationUtxo = txos.filter(_.transactionOutput.value.value.typeIdentifier == LvlType).head.outputAddress)
```

### Generate a New Lock Address

Since we will create a new transaction, we need to generate a new Lock Address to encumber the transaction outputs. Mirroring 
the [Group Constructor section](#generate-a-new-lock-address), we will use the same lock address for both the minted series 
constructor tokens and the change.

This is very similar to the linked section, however, we are using `nextInteraction=3` instead of `nextInteraction=2`. In
other words, we are deriving the verification key at path `1/1/3` (or `self/default/3`).

### Create Transaction

Using everything we generated in this section, we can finally build the transaction using the Transaction Builder API. 
We will be using the `buildSeriesMintingTransaction` function. 
Read [Mint Series Constructor Tokens](../../reference/transactions/minting#mint-series-constructor-tokens) for more information.

```scala
TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID).buildSeriesMintingTransaction(
  txos,  // The UTXOs we queried from "Query Genus Funds"
  inputLock.getPredicate, // The existing lock we retrieved from "Query Genus Funds"
  seriesPolicy, // The group policy we created from "Create Series Policy"
  5L, // The amount of Group Constructor Tokens we want to mint
  mintedAddress, // The lock address we created from "Generate a New Lock Address"
  changeAddress, // The lock address we created from "Generate a New Lock Address"
  1L // An arbitrary fee amount
)
```

### Prove and Broadcast Transaction

Mirroring the previous section, after we build the transaction, we need to prove, validate, and broadcast the transaction. 
This is identical to the [Prove Transaction](#prove-transaction) and [Broadcast Transaction](#broadcast-transaction) sections
from earlier.

### Putting it all together

Your code should look something like this:

```scala title="Mint Series Constructor Tokens"
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.Context
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.{BifrostQueryAlgebra, GenusQueryAlgebra, RpcChannelResource}
import co.topl.brambl.models.Event.SeriesPolicy
import co.topl.brambl.models.Indices
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.syntax.{LvlType, int128AsBigInt, valueToQuantitySyntaxOps, valueToTypeIdentifierSyntaxOps}
import co.topl.brambl.utils.Encoding
import co.topl.brambl.wallet.{CredentiallerInterpreter, WalletApi}
import quivr.models.VerificationKey

import java.nio.file.Paths

implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

// Replace with the desired location for your tutorial directory
val tutorialDir = Paths.get(System.getProperty("user.home"), "tutorial").toString
// Replace with the desired location for your key file
val keyFile = Paths.get(tutorialDir, "keyfile.json").toString
// Replace with the desired location of for your wallet state DB file
val walletDb = Paths.get(tutorialDir, "wallet.db").toString
val conn = WalletStateResource.walletResource(walletDb)
// Replace with the address and port of your node's gRPC endpoint
val channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)

val walletKeyApi = WalletKeyApi.make[IO]()
val walletApi = WalletApi.make(walletKeyApi)
val walletStateApi = WalletStateApi.make[IO](conn, walletApi)
val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource)
val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)

val unprovenTransaction = for {
  inputLock <- walletStateApi.getLock("self", "default", 2)
  inputAddress <- txBuilder.lockAddress(inputLock.get)
  txos <- genusQueryApi.queryUtxo(inputAddress)
  seriesPolicy = SeriesPolicy(
    "Series Policy Label",
    registrationUtxo = txos.filter(_.transactionOutput.value.value.typeIdentifier == LvlType).head.outputAddress
  )
  outputLock <- walletStateApi.getLock("self", "default", 3)
  outputAddress <- txBuilder.lockAddress(outputLock.get)
  outputVk <- walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
    // Derive the verification key at path 1/1/3 (used in outputLock)
    walletApi.deriveChildVerificationKey(VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get), 3)
  }
  _ <- walletStateApi.updateWalletState(
    Encoding.encodeToBase58(outputLock.get.getPredicate.toByteArray),
    outputAddress.toBase58(),
    Some("ExtendedEd25519"),
    Some(Encoding.encodeToBase58(outputVk.toByteArray)),
    Indices(1, 1, 3)
  )
  tx <- txBuilder.buildSeriesMintingTransaction(
    txos,
    inputLock.get.getPredicate,
    seriesPolicy,
    5L,
    outputAddress,
    outputAddress,
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
} yield ()

broadcastTransaction.unsafeRunSync()

val queryFunds = for {
  // The tokens were sent to the address derived from (self, default, 3)
  sigLock <- walletStateApi.getLock("self", "default", 3)
  sigAddress <- txBuilder.lockAddress(sigLock.get)
  txos <- genusQueryApi.queryUtxo(sigAddress)
} yield txos.map(_.transactionOutput.value.value).map(value =>
  s"${value.typeIdentifier.getClass.getSimpleName}: ${value.quantity.intValue}"
)

// Allow some time to pass before querying the transaction
Thread.sleep(15000)

queryFunds.unsafeRunSync().foreach(println)
```

After ensuring that some time has passed since minting the Group Constructor Tokens from Step 1 (15-20 seconds), try 
running the code. If all went well, you should see that you have 98 LVLs, 5 Group Constructor Token, and 5 Series 
Constructor token in your wallet, ready to be spent by you.

```bash title="output"
GroupType: 5
LvlType$: 98
SeriesType: 5
```

Keep the same local node instance running.

## Step 3: Mint Asset Tokens

Now that we have Group and Series Constructor Tokens ready to spend, we can mint our Asset Tokens. Asset Tokens are minted
using an Asset Minting Statement. For the sake of brevity, this section will reference prior sections of this tutorial where
possible. If you have not completed the prior sections, please do so before continuing.

### Query Genus Funds

From the previous section, our wallet should have 98 LVLs, 5 Group Constructor Tokens, and 5 Series Constructor Tokens 
encumbered by a 1-of-1 Signature Lock stored at (fellowship="self", template="default", nextInteraction=3). To obtain the
funds, we must query Genus for the UTXOs. This is very similar to the [previous section](#query-genus-funds-1), however, we are using `nextInteraction=3`
instead of `nextInteraction=2`.

### Create Asset Minting Statement

We need to create the Asset Minting Statement that will be used to mint the Asset Tokens. To see all fields of an
Asset Minting Statement, see [TIP-0003](https://github.com/Topl/tips/tree/main/TIP-0003#asset-minting-statement).

Similar to the Group and Series Policy, the Asset Minting Statement reference existing UTXOs. However, these UTXOs should not
contain LVLs. The field `groupTokenUtxo` should reference a UTXO that contains the Group Constructor Tokens and the field
`seriesTokenUtxo` should reference a UTXO that contains the Series Constructor Tokens. In this tutorial, we will use the 
UTXO that contains the 5 Group Constructor Tokens and the UTXO that contains the 5 Series Constructor Tokens, respectively.
Since all of our UTXOs from the previous sections are encumbered by the same Lock Address, we will need to extract the TXOs
with the relevant constructor tokens.

For example:

```scala
AssetMintingStatement(
  txos.filter(_.transactionOutput.value.value.isGroup).head.outputAddress,
  txos.filter(_.transactionOutput.value.value.isSeries).head.outputAddress,
  BigInt(10)
)
```

### Generate a New Lock Address

Since we will create a new transaction, we need to generate a new Lock Address to encumber the transaction outputs. Mirroring
the [Group Constructor section](#generate-a-new-lock-address) and [Series Constructor section](#generate-a-new-lock-address-1), 
we will use the same lock address for both the minted asset tokens and the change.

This is very similar to the linked sections, however, we are using `nextInteraction=4`. In other words, we are deriving 
the verification key at path `1/1/4` (or `self/default/4`).

### Create Transaction

Using everything we generated in this section, we can finally build the transaction using the Transaction Builder API.
We will be using the `buildAssetMintingTransaction` function.
Read [Mint Asset Tokens](../../reference/transactions/minting#mint-asset-tokens) for more information.

```scala
TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID).buildAssetMintingTransaction(
  mintingStatement, // The Asset Minting Statement we created from "Create Asset Minting Statement"
  txos,  // The UTXOs we queried from "Query Genus Funds"
  Map(inputAddr -> inputLock.getPredicate), // A mapping containing the existing lock we retrieved from "Query Genus Funds"
  1L, // An arbitrary fee amount
  mintedAddress, // The lock address we created from "Generate a New Lock Address"
  changeAddress, // The lock address we created from "Generate a New Lock Address"
)
```

:::note
Unlike previous sections, this function takes in a mapping of input addresses to their respective locks. This is because
the Group Constructor Tokens, Series Constructor Tokens, and LVLs (for fee) could potentially all be encumbered by different
locks. In this tutorial, we are using the same lock for all three, but this is not a requirement.
:::

### Prove and Broadcast Transaction

Mirroring the previous section, after we build the transaction, we need to prove, validate, and broadcast the transaction.
This is identical to the [Prove Transaction](#prove-transaction) and [Broadcast Transaction](#broadcast-transaction) sections
from earlier.

### Putting it all together

At this point, your code should look something like this:

```scala title="Mint Asset Tokens"
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.Context
import co.topl.brambl.builders.TransactionBuilderApi
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.{BifrostQueryAlgebra, GenusQueryAlgebra, RpcChannelResource}
import co.topl.brambl.models.Indices
import co.topl.brambl.models.box.AssetMintingStatement
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.syntax.{bigIntAsInt128, int128AsBigInt, valueToQuantitySyntaxOps, valueToTypeIdentifierSyntaxOps}
import co.topl.brambl.utils.Encoding
import co.topl.brambl.wallet.{CredentiallerInterpreter, WalletApi}
import quivr.models.VerificationKey

import java.nio.file.Paths

implicit val transformType: FunctionK[IO, IO] = FunctionK.id[IO]

// Replace with the desired location for your tutorial directory
val tutorialDir = Paths.get(System.getProperty("user.home"), "tutorial").toString
// Replace with the desired location for your key file
val keyFile = Paths.get(tutorialDir, "keyfile.json").toString
// Replace with the desired location of for your wallet state DB file
val walletDb = Paths.get(tutorialDir, "wallet.db").toString
val conn = WalletStateResource.walletResource(walletDb)
// Replace with the address and port of your node's gRPC endpoint
val channelResource = RpcChannelResource.channelResource[IO]("localhost", 9084, secureConnection = false)

val walletKeyApi = WalletKeyApi.make[IO]()
val walletApi = WalletApi.make(walletKeyApi)
val walletStateApi = WalletStateApi.make[IO](conn, walletApi)
val genusQueryApi = GenusQueryAlgebra.make[IO](channelResource)
val txBuilder = TransactionBuilderApi.make[IO](PRIVATE_NETWORK_ID, MAIN_LEDGER_ID)

val unprovenTransaction = for {
  inputLock <- walletStateApi.getLock("self", "default", 3)
  inputAddress <- txBuilder.lockAddress(inputLock.get)
  txos <- genusQueryApi.queryUtxo(inputAddress)
  mintingStatement = AssetMintingStatement(
    txos.filter(_.transactionOutput.value.value.isGroup).head.outputAddress,
    txos.filter(_.transactionOutput.value.value.isSeries).head.outputAddress,
    BigInt(10)
  )
  outputLock <- walletStateApi.getLock("self", "default", 4)
  outputAddress <- txBuilder.lockAddress(outputLock.get)
  outputVk <- walletStateApi.getEntityVks("self", "default").map(_.get.head) flatMap { vk =>
    // Derive the verification key at path 1/1/4 (used in outputLock)
    walletApi.deriveChildVerificationKey(VerificationKey.parseFrom(Encoding.decodeFromBase58(vk).toOption.get), 4)
  }
  _ <- walletStateApi.updateWalletState(
    Encoding.encodeToBase58(outputLock.get.getPredicate.toByteArray),
    outputAddress.toBase58(),
    Some("ExtendedEd25519"),
    Some(Encoding.encodeToBase58(outputVk.toByteArray)),
    Indices(1, 1, 4)
  )
  tx <- txBuilder.buildAssetMintingTransaction(
    mintingStatement,
    txos,
    Map(inputAddress -> inputLock.get.getPredicate),
    1L,
    outputAddress,
    outputAddress
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
} yield ()

broadcastTransaction.unsafeRunSync()

val queryFunds = for {
  // The tokens were sent to the address derived from (self, default, 4)
  sigLock <- walletStateApi.getLock("self", "default", 4)
  sigAddress <- txBuilder.lockAddress(sigLock.get)
  txos <- genusQueryApi.queryUtxo(sigAddress)
} yield txos.map(_.transactionOutput.value.value).map(value =>
  s"${value.typeIdentifier.getClass.getSimpleName}: ${value.quantity.intValue}"
)

// Allow some time to pass before querying the transaction
Thread.sleep(15000)

queryFunds.unsafeRunSync().foreach(println)
```

After ensuring that some time has passed since minting the Series Constructor Tokens from Step 2 (15-20 seconds), try
running the code. If all went well, you should see that you have 97 LVLs, 5 Group Constructor Token, 5 Series Constructor
Token, and 10 Asset token in your wallet, ready to be spent by you.

```bash title="output"
SeriesType: 5
GroupType: 5
LvlType$: 97
AssetType: 10
```

:::note
The above output will differ if you configured your Series Policy differently.
:::

For example, if you configured your Series Policy's `tokenSupply` to be 5, you should see that you have 3 Series 
Constructor Tokens remaining instead of 5. This is because setting the `tokenSupply` field indicates that the Series 
Constructor Tokens have a limited supply. In other words, a Series Constructor Token is burned for every 5 Asset Tokens 
minted, leaving 3 remaining as change. For more information on the possible configurations of a Series Policy, see
[TIP-0003](https://github.com/Topl/tips/tree/main/TIP-0003).