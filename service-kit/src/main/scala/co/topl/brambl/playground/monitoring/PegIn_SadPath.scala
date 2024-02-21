package co.topl.brambl.playground.monitoring

import co.topl.brambl.playground.{mineBlocks, User}

object PegIn_SadPath extends App {
  // Initiate Sally's wallet
  println("Initiating Sally's wallet")
  val sally = User("sally")

  // initiate peg-in
  println("Sally initiating peg-in")
  val peginResponse = sally.initiatePegIn()

  // Sally sends BTC to the peg-in descriptor
  println("Sally sends BTC to the peg-in descriptor")
  sally.sendBtcToDesc(peginResponse.desc)

  // Sally reclaims BTC
  println("Sally waiting 1000 blocks to reclaim BTC") // Sally can only reclaim after 1000 blocks
  mineBlocks(1000)
  sally.reclaimBtc(peginResponse.desc)
}
