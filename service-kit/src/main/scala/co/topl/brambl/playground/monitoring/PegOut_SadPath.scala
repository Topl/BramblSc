package co.topl.brambl.playground.monitoring

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.playground.{ec, genusQueryApi, rpcCli, User}

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
  bob.claimTBtc(peginResponse.toplAddress)

  (IO
    .fromFuture(
      IO(rpcCli.listUnspent("bob-watcher").map(_.find(_.address.contains(peginResponse.bitcoinAddress))))
    )
    .iterateWhile(_.isDefined) *> IO.println("BTC is claimed!")).unsafeRunSync()
  println("Bob's balance after reclaiming TBTC:")
  bob.displayBalance()
}
