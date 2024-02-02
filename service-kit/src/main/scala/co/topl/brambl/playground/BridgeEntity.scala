package co.topl.brambl.playground

trait BridgeEntity extends BitcoinWallet with ToplWallet {
  override val walletName: String
}
