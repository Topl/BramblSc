import cats.effect.unsafe.implicits.global
import co.topl.brambl.playground.monitoring.BridgeDemo

import scala.language.implicitConversions


//Example.run(List.empty[String]).unsafeRunSync()
BridgeDemo.run(List.empty[String]).unsafeRunSync()