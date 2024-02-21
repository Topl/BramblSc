package co.topl.brambl.playground.monitoring

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.playground.{User, ec, genusQueryApi, rpcCli}

object PegInOut_HappyPath extends App {
  // Initiate Alice's wallet
  println("Initiating Alice's wallet")
  val alice = User("alice")

  // initiate peg-in
  println("Alice initiating peg-in")
  val peginResponse = alice.initiatePegIn()
  println("Alice sends BTC to the peg-in descriptor")
  alice.sendBtcToDesc(peginResponse.desc)
  println("Alice waits for the TBTC to be minted")
  (
    genusQueryApi.queryUtxo(peginResponse.toplAddress).iterateWhile(_.isEmpty) *>
    IO.println("tBTC funded!")
  ).unsafeRunSync()
  println("Alice claims TBTC")
  // Alice notifies bridge of the claim since it is not automated yet
  val txId = alice.claimTBtc(peginResponse.toplAddress)
  alice.notifyBridgeOfTbtcClaim(txId, peginResponse.toplAddress)

  // initiate peg-out
  println("Alice initiating peg-out")
  val pegoutResponse = alice.initiatePegOut()
  println("Alice sends TBTC to the peg-out address")
  alice.sendTbtcToAddress(pegoutResponse.toplLock)
  println("Alice waits for the TBTC to be transferred")
  (
    IO.fromFuture(
      IO(rpcCli.listUnspent("alice-watcher").map(_.find(_.address.contains(pegoutResponse.bitcoinAddress))))
    ).iterateWhile(_.isEmpty)
    *> IO.println("BTC transferred!")
  ).unsafeRunSync()
  println("Alice claims BTC")
  alice.claimBtc(pegoutResponse.desc)

  // TODO: Loop until TBTC claimed, then print alice's balance again
}