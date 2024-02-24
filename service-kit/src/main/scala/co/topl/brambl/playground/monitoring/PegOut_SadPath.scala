package co.topl.brambl.playground.monitoring

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.playground.{ec, genusQueryApi, rpcCli, User}

object PegOut_SadPath extends App {
  // Initiate Bob's wallet
  println("Initiating Bob's wallet")
  val bob = User("bob")
  println("Bob's balance before peg-in:")
  bob.displayBalance()

  // initiate peg-in
  println("Bob initiating peg-in")
  val peginResponse = bob.initiatePegIn()
  // Bob sends BTC to the peg-in descriptor
  println("Bob sends BTC to the peg-in descriptor")
  bob.sendBtcToDesc(peginResponse.desc)
  println("Bob waits for the TBTC to be minted")

  (genusQueryApi.queryUtxo(peginResponse.toplAddress).iterateWhile(_.isEmpty) *> IO.println("tBTC funded!"))
    .unsafeRunSync()
  println("Bob claims TBTC")
  bob.claimTBtc(peginResponse.toplAddress)

  println("Alice waits until bridge has BTC before initiating peg-out")

  (IO
    .fromFuture(
      IO(rpcCli.listUnspent(bob.btcWallet.watcherName).map(_.find(_.address.contains(peginResponse.bitcoinAddress))))
    )
    .iterateWhile(_.isDefined) *> IO.println("BTC is claimed!")).unsafeRunSync()
  println("Bob's balance before peg-out:")
  bob.displayBalance()

  // initiate peg-out
  println("Bob initiating peg-out")
  val pegoutResponse = bob.initiatePegOut()
  println("Bob sends TBTC to the peg-out address")
  bob.sendTbtcToAddress(pegoutResponse.toplLock)
  println("Bob waits for the BTC to be transferred")

  (IO
    .fromFuture(
      IO(rpcCli.listUnspent(bob.btcWallet.watcherName).map(_.find(_.address.contains(pegoutResponse.bitcoinAddress))))
    )
    .iterateWhile(_.isEmpty) *> IO.println("BTC transferred!")).unsafeRunSync()
  println("Bob reclaims TBTC")
  bob.reclaimTbtc(pegoutResponse.toplAddress)

  (IO
    .fromFuture(
      IO(rpcCli.listUnspent(bob.btcWallet.watcherName).map(_.find(_.address.contains(pegoutResponse.bitcoinAddress))))
    )
    .iterateWhile(_.nonEmpty) *> IO.println("BTC is claimed!")).unsafeRunSync()

  println("Bob's balance after reclaiming TBTC:")
  bob.displayBalance()
}
