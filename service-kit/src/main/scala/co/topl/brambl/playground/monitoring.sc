import cats.effect.unsafe.implicits.global
import co.topl.brambl.playground.monitoring.Example

import scala.language.implicitConversions


Example.run(List.empty[String]).unsafeRunSync()