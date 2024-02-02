package co.topl.brambl.playground

import co.topl.brambl.models.Indices
import org.bitcoins.core.crypto.ExtPrivateKey
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.crypto.ECPrivateKey

trait BitcoinWallet {
  val walletName: String
  def initBtcFunds(): Unit = {}

  private val watcherName: String = s"$walletName-watcher"

  private def createBitcoinWallets(): Unit = {
    println(s"Creating wallets $walletName and $watcherName")
    handleCall(rpcCli.createWallet(walletName, descriptors = true))
    handleCall(rpcCli.createWallet(watcherName, descriptors = true, disablePrivateKeys = true))
    initBtcFunds()
  }

  createBitcoinWallets()

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

  // BTC version of our wallet state
  private case class CartesianEntry(idx: Indices, desc: String, address: String)
  var cartesianIndexing: Seq[CartesianEntry] = Seq()

  def addWalletEntry(idx: Indices, desc: String, address: String): Unit =
    cartesianIndexing = cartesianIndexing :+ CartesianEntry(idx, desc, address)

  def getIndicesByDesc(desc: String): Indices =
    cartesianIndexing.find(_.desc == desc).get.idx

  // Storing desc => txOut since ATM we can't query bitcoin core just using desc
  private case class DescTxOut(desc: String, txOut: String)
  var descTxOutIndexing: Seq[DescTxOut] = Seq()

  def addDescTxOutEntry(desc: String, txOut: String): Unit =
    descTxOutIndexing = descTxOutIndexing :+ DescTxOut(desc, txOut)

  def getTxOut(desc: String): String =
    descTxOutIndexing.find(_.desc == desc).get.txOut

  def getDesc(txOut: String): String =
    descTxOutIndexing.find(_.txOut == txOut).get.desc

}
