package co.topl.brambl.playground.monitoring

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.playground.{User, genusQueryApi}

object PegInOut_HappyPath extends App {
  // Initiate Alice's wallet
  println("Initiating Alice's wallet")
  val alice = User("alice")

  // initiate peg-in
  println("Alice initiating peg-in")
  val peginResponse = alice.initiatePegIn()

  // Alice sends BTC to the peg-in descriptor
  println("Alice sends BTC to the peg-in descriptor")
  alice.sendBtcToDesc(peginResponse.desc)

  // Once TBTC is minted, Alice claims it
  println("Alice waits for the TBTC to be minted")
  (genusQueryApi.queryUtxo(peginResponse.toplAddress).iterateWhile(_.isEmpty) *>
    IO.println("tBTC funded!")).unsafeRunSync()
  println("Alice claims TBTC")
  val txId = alice.claimTBtc(peginResponse.toplAddress)
  // Alice notifies bridge of the claim since it is not automated yet
  alice.notifyBridgeOfTbtcClaim(txId, peginResponse.toplAddress)
}