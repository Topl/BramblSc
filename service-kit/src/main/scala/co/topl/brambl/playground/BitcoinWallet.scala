package co.topl.brambl.playground

import co.topl.brambl.models.{Indices, LockAddress}
import org.bitcoins.core.crypto.ExtPrivateKey
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.crypto.ECPrivateKey

class BitcoinWallet(val walletName: String) {
  def initBtcFunds(): Unit = {}

  val watcherName: String = s"$walletName-watcher"

  private def createBitcoinWallets(): Unit = {
    println(s"Creating wallets $walletName and $watcherName")
    handleCall(rpcCli.createWallet(walletName, descriptors = true))
    handleCall(rpcCli.createWallet(watcherName, descriptors = true, disablePrivateKeys = true))
    initBtcFunds()
  }

  createBitcoinWallets()

  def getBalance(): Unit = {
    def printBalance(wallet: String): String =
      Seq(
        s"Balance of $wallet: ",
        formatBalances(handleCall(rpcCli.getBalances(wallet)).get.mine),
        s"# of spendable UTXOs of $wallet: ${handleCall(rpcCli.listUnspent(wallet)).get.length}"
      ).mkString("\n")

    Seq(
        walletName,
        watcherName
        ) map printBalance mkString("\n", "\n", "\n")
  }

  private def getBtcMainKey(): ExtPrivateKey = {
    val rootSecretKeyRaw =
      (handleCall(rpcCli.listDescriptors(walletName, isPrivate = true)).get.head \ "desc").result.get.toString()
    val rootSecretKey = ExtPrivateKey.fromString(
      rootSecretKeyRaw.substring(rootSecretKeyRaw.indexOf("(") + 1, rootSecretKeyRaw.indexOf("/"))
    )
    // m / purpose' / coin_type' / account' / change / index ... BIP-044
    // our existing indices do not follow this scheme, so we need to choose a purpose that is not already in use
    // Per Bip-43 (and other Bips), purposes known to be in use are (non-exhaustive): 0, 44, 49, 86, 84, (1852 for cardano ed25519)
    // For now, we will choose 7091 (which is our coin_type, but not recognized since our purpose is not 44)
    val mainKeyPath = BIP32Path.fromString("m/7091'")
    rootSecretKey.deriveChildPrivKey(mainKeyPath)
  }
  val mainKeyBtc: ExtPrivateKey = getBtcMainKey()

  def getChildSecretKey(idx: Indices): ECPrivateKey = {
    val keyPath = s"m/${idx.x}'/${idx.y}'/${idx.z}"
    println(s"Generating Bitcoin child key pair for $walletName at $keyPath...")
    val childPath = BIP32Path.fromString(keyPath)
    mainKeyBtc.deriveChildPrivKey(childPath).key
  }

  def createToWalletTx(
    utxoToSpend:   TransactionOutPoint,
    spendTimeLock: Boolean = false
  ): Transaction = {
    println(s"using utxoToSpend: $utxoToSpend")
    println(walletName)
    val toAddr = handleCall(rpcCli.getNewAddress(walletNameOpt = Some(walletName)), debug = true).get
    println(s"toAddr: $toAddr")
    val inputAmount = handleCall(rpcCli.getTxOut(utxoToSpend.txIdBE, utxoToSpend.vout.toLong), debug = true).get.value
    println(s"inputAmount: $inputAmount")
    createBaseTx(utxoToSpend.txIdBE, utxoToSpend.vout, toAddr, inputAmount, spendTimeLock)
  }

  def sendFromWallet(recipientAddr: String, toSend: Option[BigInt]): TransactionOutPoint = {
    val desiredAmount = toSend.map(sat => Bitcoins(Satoshis(sat)))
    val initialFundsUtxo = handleCall(rpcCli.listUnspent(walletName)).get.head
    val unprovenTx = createBaseTx(
      initialFundsUtxo.txid,
      UInt32(initialFundsUtxo.vout),
      BitcoinAddress(recipientAddr),
      // If the request amount does not exceed the available funds, use it, otherwise use all available funds
      desiredAmount.filter(btc => btc <= initialFundsUtxo.amount).getOrElse(initialFundsUtxo.amount)
    )
    val provenTx = handleCall(rpcCli.signRawTransactionWithWallet(unprovenTx, Some(walletName))).get.hex
    val txId = handleCall(rpcCli.sendRawTransaction(provenTx, 0)).get
    mineBlocks(1)
    TransactionOutPoint(txId, UInt32(0))
  }

  def sendBtcToDesc(desc: String, amount: Option[BigInt] = None): String = {
    print("\n============================" + s"$walletName sends BTC to Descriptor" + "============================\n")
    println(s"> $walletName deriving address from descriptor...")
    val address = handleCall(rpcCli.deriveOneAddress(walletName, desc)).get
    println(s"> $walletName sends BTC to address...")
    sendFromWallet(address, amount).toHumanReadableString
  }

  // BTC version of our wallet state
  private case class CartesianEntry(idx: Indices, desc: String, lockAddr: LockAddress)
  private var cartesianIndexing: Seq[CartesianEntry] = Seq()

  def addWalletEntry(idx: Indices, desc: String, lockAddr: LockAddress): Unit =
    cartesianIndexing = cartesianIndexing :+ CartesianEntry(idx, desc, lockAddr)

  def getIndicesByDesc(desc: String): Indices =
    cartesianIndexing.find(_.desc == desc).get.idx

  def getDescByIndices(idx: Indices): String =
    cartesianIndexing.find(_.idx == idx).get.desc

  def getDescByAddress(lockAddr: LockAddress): String =
    cartesianIndexing.find(_.lockAddr == lockAddr).get.desc

  // Storing desc => txOut since ATM we can't query bitcoin core just using desc
  private case class DescTxOut(desc: String, txOut: String)
  private var descTxOutIndexing: Seq[DescTxOut] = Seq()

  def addDescTxOutEntry(desc: String, txOut: String): Unit =
    descTxOutIndexing = descTxOutIndexing :+ DescTxOut(desc, txOut)

  def getTxOut(desc: String): String =
    descTxOutIndexing.find(_.desc == desc).get.txOut

  def getDesc(txOut: String): String =
    descTxOutIndexing.find(_.txOut == txOut).get.desc

}
