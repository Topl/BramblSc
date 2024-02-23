package co.topl.brambl.playground.monitoring

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.playground.{ec, genusQueryApi, rpcCli, User}

object PegInOut_HappyPath extends App {
  // Initiate Alice's wallet
  println("Initiating Alice's wallet")
  val alice = User("alice")
  println("Alice's balance before peg-in:")
  alice.displayBalance()

  // initiate peg-in
  println("Alice initiating peg-in")
  val peginResponse = alice.initiatePegIn()
  println("Alice sends BTC to the peg-in descriptor")
  alice.sendBtcToDesc(peginResponse.desc)
  println("Alice waits for the TBTC to be minted")

  (genusQueryApi.queryUtxo(peginResponse.toplAddress).iterateWhile(_.isEmpty) *> IO.println("tBTC funded!"))
    .unsafeRunSync()
  println("Alice claims TBTC")
  alice.claimTBtc(peginResponse.toplAddress)

  println("Alice waits until bridge has BTC before initiating peg-out")

  (IO
    .fromFuture(
      IO(rpcCli.listUnspent(alice.btcWallet.watcherName).map(_.find(_.address.contains(peginResponse.bitcoinAddress))))
    )
    .iterateWhile(_.isDefined) *> IO.println("BTC is claimed!")).unsafeRunSync()
  println("Alice's balance before peg-out:")
  alice.displayBalance()

  // initiate peg-out
  println("Alice initiating peg-out")
  val pegoutResponse = alice.initiatePegOut()
  println("Alice sends TBTC to the peg-out address")
  alice.sendTbtcToAddress(pegoutResponse.toplLock)
  println("Alice waits for the TBTC to be transferred")

  (IO
    .fromFuture(
      IO(rpcCli.listUnspent(alice.btcWallet.watcherName).map(_.find(_.address.contains(pegoutResponse.bitcoinAddress))))
    )
    .iterateWhile(_.isEmpty) *> IO.println("BTC transferred!")).unsafeRunSync()
  println("Alice claims BTC")
  alice.claimBtc(pegoutResponse.desc)

  (genusQueryApi.queryUtxo(pegoutResponse.toplAddress).iterateWhile(_.nonEmpty) *> IO.println("tBTC is claimed!"))
    .unsafeRunSync()
  println("Alice's balance after peg-out:")
  alice.displayBalance()
}
