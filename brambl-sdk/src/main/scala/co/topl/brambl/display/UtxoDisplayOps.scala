package co.topl.brambl.display

import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.transaction.UnspentTransactionOutput

trait UtxoDisplayOps {

  implicit val utxoDisplay: DisplayOps[UnspentTransactionOutput] = (utxo: UnspentTransactionOutput) =>
    Seq(
      padLabel("LockAddress") + utxo.address.display,
      utxo.value.value.display
    ).mkString("\n")

  implicit val lockAddressDisplay: DisplayOps[LockAddress] = (lockAddress: LockAddress) =>
    AddressCodecs.encodeAddress(lockAddress)

}
