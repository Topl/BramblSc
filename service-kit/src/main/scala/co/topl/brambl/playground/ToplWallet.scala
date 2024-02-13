package co.topl.brambl.playground

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.constants.NetworkConstants.{MAIN_LEDGER_ID, PRIVATE_NETWORK_ID}
import co.topl.brambl.dataApi.WalletStateAlgebra
import co.topl.brambl.models.{Indices, LockAddress}
import co.topl.brambl.servicekit.{WalletKeyApi, WalletStateApi, WalletStateResource}
import co.topl.brambl.syntax.{int128AsBigInt, valueToQuantitySyntaxOps, AssetType, LvlType}
import co.topl.brambl.wallet.{Credentialler, CredentiallerInterpreter, WalletApi}
import quivr.models.{KeyPair, VerificationKey}

import java.io.File
import java.nio.file.Paths

class ToplWallet(val walletName: String) {

  private val ToplDir = Paths.get(System.getProperty("user.home"), "btc-example").toString
  println(s"ToplDir: $ToplDir")
  println(s"walletName: $walletName")
  val toplDir: String = Paths.get(ToplDir, walletName).toString
  new File(toplDir).mkdirs() // Create the directory if it doesn't exist

  private def initFilePath(fileName: String): String = {
    val filePath = Paths.get(toplDir, fileName).toString
    new File(filePath).delete() // Clear the file if it already exists
    filePath
  }

  private def initializeToplWallet(
    walletApi:      WalletApi[IO],
    walletStateApi: WalletStateAlgebra[IO],
    keyFile:        String,
    mnemonic:       String
  ): KeyPair = {
    val mainKey = (for {
      walletResult <- walletApi.createAndSaveNewWallet[IO]("password".getBytes, name = keyFile, mnemonicName = mnemonic)
      mainKeyPair  <- walletApi.extractMainKey(walletResult.toOption.get.mainKeyVaultStore, "password".getBytes())
      _            <- walletStateApi.initWalletState(PRIVATE_NETWORK_ID, MAIN_LEDGER_ID, mainKeyPair.toOption.get)
    } yield mainKeyPair.toOption.get).unsafeRunSync()
    mainKey
  }

  val walletApi: WalletApi[IO] = WalletApi.make[IO](WalletKeyApi.make[IO]())

  val walletStateApi: WalletStateAlgebra[IO] =
    WalletStateApi.make[IO](WalletStateResource.walletResource(initFilePath("wallet.db")), walletApi)

  val mainKeyTopl: KeyPair =
    initializeToplWallet(walletApi, walletStateApi, initFilePath("keyfile.json"), initFilePath("mnemonic.txt"))
  val credentialler: Credentialler[IO] = CredentiallerInterpreter.make[IO](walletApi, walletStateApi, mainKeyTopl)

  private def loadLvls(): Unit = {
    val loadFunds = for {
      inLock <- walletStateApi.getLock("nofellowship", "genesis", 1)
      inAddr <- txBuilder.lockAddress(inLock.get)
      txos   <- genusQueryApi.queryUtxo(inAddr)
      outputAddr <- walletStateApi
        .getLock("self", "default", 1)
        .flatMap(lock => txBuilder.lockAddress(lock.get))
      unprovenTx <- txBuilder.buildTransferAmountTransaction(
        LvlType,
        txos,
        inLock.get.getPredicate,
        100L,
        outputAddr,
        inAddr,
        1L
      )
      provenTx <- credentialler.prove(unprovenTx.toOption.get)
      txId     <- bifrostQuery.broadcastTransaction(provenTx)
    } yield txId
    loadFunds.unsafeRunSync()
  }

  def initToplFunds(): Option[AssetType] = {
    println(s"Initializing 100Lvls to $walletName ... waiting 30 secs")
    loadLvls()
    Thread.sleep(30000)
    None
  }

  def getChildVk(idx: Indices): VerificationKey = {
    println(s"Generating Topl child key pair for $walletName at $idx...")
    walletApi.deriveChildKeys(mainKeyTopl, idx).unsafeRunSync().vk
  }

  def getTbtcBalance(lockAddr: LockAddress): BigInt =
    genusQueryApi
      .queryUtxo(lockAddr)
      .unsafeRunSync()
      .map(_.transactionOutput.value.value.quantity: BigInt)
      .fold(BigInt(0))(_ + _)

}
