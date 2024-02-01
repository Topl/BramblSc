package co.topl.brambl.playground

import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.brambl.models.box.Lock
import co.topl.brambl.playground.BridgeQuery.{PegInRequest, PegInResponse}
import quivr.models.VerificationKey

case class BridgeQuery(bridge: Bridge) {

  def initiatePegInRequest(request: PegInRequest): PegInResponse =
    bridge.handleRequest(request.hash, request.bitcoinPk, request.toplVk)

  def notifyOfBtcTransfer(txOut: String): Unit =
    bridge.triggerMinting(txOut)

  def notifyOfTbtcClaim(txId: TransactionId): Unit =
    bridge.claimBtc(txId)
}

object BridgeQuery {

  case class PegInRequest(
    hash:      String,
    bitcoinPk: String,
    toplVk:    VerificationKey
  )

  case class PegInResponse(
    desc:        String,
    toplLock:    Lock,
    toplAddress: LockAddress // Serves as a checksum for the toplLock
  )
}
