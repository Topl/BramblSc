package co.topl.brambl.playground.monitoring

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.brambl.playground.{User, genusQueryApi, mineBlocks}

object PegIn_SadPath extends App {
  // Initiate Sally's wallet
  println("Initiating Sally's wallet")
  val sally = User("sally")
  println("Sally's balance before peg-in:")
  sally.displayBalance()

  // initiate peg-in
  println("Sally initiating peg-in")
  val peginResponse = sally.initiatePegIn()
  // Sally sends BTC to the peg-in descriptor
  println("Sally sends BTC to the peg-in descriptor")
  sally.sendBtcToDesc(peginResponse.desc)
  // Sally reclaims BTC
  println("Sally waiting 1000 blocks (and bridge minting) to reclaim BTC") // Sally can only reclaim after 1000 blocks
  mineBlocks(1000)
  (genusQueryApi.queryUtxo(peginResponse.toplAddress).iterateWhile(_.isEmpty) *> IO.println("tBTC is minted!")).unsafeRunSync()
  sally.reclaimBtc(peginResponse.desc)

  (genusQueryApi.queryUtxo(peginResponse.toplAddress).iterateWhile(_.nonEmpty) *> IO.println("tBTC is claimed!")).unsafeRunSync()
  println("Sally's balance after reclaiming BTC:")
  sally.displayBalance()
}
