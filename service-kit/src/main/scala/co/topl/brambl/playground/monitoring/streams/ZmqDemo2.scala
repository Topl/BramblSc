package co.topl.brambl.playground.monitoring.streams

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.bitcoins.zmq.ZMQSubscriber

import java.net.InetSocketAddress
import scala.concurrent.duration.DurationInt

object ZmqDemo2 extends App {
  // initialize stream from specific block
  val subscriber = new ZMQSubscriber(
    new InetSocketAddress("127.0.0.1", 28332),
    None,
    None,
    Some(tx => {
      println(tx)
      // add it to a queue
      // infinite stream, either waits or gets an element or it returns the latest element
      //          addTxToQueue(queue, _)
    }),
    // look into chain reorgs
    Some(block => { // block listener,  when the chain tip is updated. (when assumeutxo is not in use), will also issue when historical blocks are connected to the background validation chainstate
      println(block)
    })
  )
subscriber.start()
IO.unit.andWait(5.seconds).foreverM.unsafeRunSync()

}


