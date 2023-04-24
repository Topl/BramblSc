package co.topl.brambl.syntax

import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.TransactionOutputAddress

import scala.language.implicitConversions

trait TransactionIdSyntax {

  implicit def transactionIdAsIdSyntaxOps(id: TransactionId): TransactionIdSyntaxOps =
    new TransactionIdSyntaxOps(id)
}

class TransactionIdSyntaxOps(val id: TransactionId) extends AnyVal {

  def outputAddress(network: Int, ledger: Int, index: Int): TransactionOutputAddress =
    TransactionOutputAddress(network, ledger, index, id)
}
