package co.topl.brambl.display

import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.utils.Encoding
import co.topl.genus.services.Txo

trait StxoDisplayOps {

  implicit val stxoDisplay: DisplayOps[SpentTransactionOutput] = (stxo: SpentTransactionOutput) =>
    Seq(
      padLabel("TxoAddress") + stxo.address.display,
      padLabel("Attestation") + "Not implemented",
      stxo.value.value.display
    ).mkString("\n")

  implicit val txoAddressDisplay: DisplayOps[TransactionOutputAddress] = (txoAddress: TransactionOutputAddress) =>
    s"${Encoding.encodeToBase58(txoAddress.id.value.toByteArray())}#${txoAddress.index}"

  implicit val txoDisplay: DisplayOps[Txo] = (txo: Txo) =>
    Seq(
      padLabel("TxoAddress") + txo.outputAddress.display,
      padLabel("LockAddress") + txo.transactionOutput.address.display,
      txo.transactionOutput.value.value.display
    ).mkString("\n")

}
