package co.topl.brambl.display

import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.utils.Encoding

trait StxoDisplayOps {

  implicit val stxoDisplay: DisplayOps[SpentTransactionOutput] = (stxo: SpentTransactionOutput) => s"""
TxoAddress  : ${stxo.address.display}
Attestation  : Not implemented
${stxo.value.value.display}
"""

  implicit val txoAddressDisplay: DisplayOps[TransactionOutputAddress] = (txoAddress: TransactionOutputAddress) =>
    s"${Encoding.encodeToBase58(
        txoAddress.id.value.toByteArray()
      )}#${txoAddress.index}"

}
