package co.topl.brambl.playground.monitoring

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.playground.{genusQueryApi, User}

object PegOut_SadPath extends App {
  // Initiate Bob's wallet
  println("Initiating Bob's wallet")
  val bob = User("bob")

  // initiate peg-in
  println("Bob initiating peg-in")
  val peginResponse = bob.initiatePegIn()

  // Bob sends BTC to the peg-in descriptor
  println("Alice sends BTC to the peg-in descriptor")
  bob.sendBtcToDesc(peginResponse.desc)

  // Once TBTC is minted, Bob claims it
  println("Bob waits for the TBTC to be minted")

  (genusQueryApi.queryUtxo(peginResponse.toplAddress).iterateWhile(_.isEmpty) *>
  IO.println("tBTC funded!")).unsafeRunSync()
  println("Bob claims TBTC")
  val txId = bob.claimTBtc(peginResponse.toplAddress)
  // Bob notifies bridge of the claim since it is not automated yet
  bob.notifyBridgeOfTbtcClaim(txId, peginResponse.toplAddress)
}
