package co.topl.brambl.display

import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.models.{Datum, TransactionId}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.utils.Encoding

trait TransactionDisplayOps {

  implicit val transactionIdDisplay: DisplayOps[TransactionId] = (id: TransactionId) =>
    Encoding.encodeToBase58(id.value.toByteArray())

  implicit val transactionDisplay: DisplayOps[IoTransaction] = (tx: IoTransaction) =>
    s"""
TransactionId : ${tx.transactionId.getOrElse(tx.computeId).display}

Group Policies
==============

${tx.groupPolicies.map(gp => gp.display).mkString("\n-----------\n")}

Series Policies
===============

${tx.seriesPolicies.map(sp => sp.display).mkString("\n-----------\n")}

Asset Minting Statements
========================

${tx.mintingStatements.map(ams => ams.display).mkString("\n-----------\n")}


Inputs
======
${if (tx.inputs.isEmpty) ("No inputs")
      else tx.inputs.map(stxo => stxo.display).mkString("\n-----------\n")}

Outputs
=======
${if (tx.outputs.isEmpty) ("No outputs")
      else tx.outputs.map(utxo => utxo.display).mkString("\n-----------\n")}
Datum        :
${tx.datum.display}
"""

  implicit val txDatumDisplay: DisplayOps[Datum.IoTransaction] = (datumIoTransation: Datum.IoTransaction) =>
    s"""
Value      : ${Encoding.encodeToBase58(
        datumIoTransation.event.metadata.value.toByteArray()
      )}
"""

}
