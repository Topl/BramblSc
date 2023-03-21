package co.topl.brambl.syntax

import co.topl.brambl.models.Identifier
import co.topl.brambl.models.TransactionOutputAddress

import scala.language.implicitConversions

trait TransactionIdSyntax {

  implicit def transactionIdAsIdSyntaxOps(id: Identifier.IoTransaction32): TransactionIdSyntaxOps =
    new TransactionIdSyntaxOps(id)
}

class TransactionIdSyntaxOps(val id: Identifier.IoTransaction32) extends AnyVal {

  def outputAddress(network: Int, ledger: Int, index: Int): TransactionOutputAddress =
    TransactionOutputAddress(network, ledger, index, TransactionOutputAddress.Id.IoTransaction32(id))
}
