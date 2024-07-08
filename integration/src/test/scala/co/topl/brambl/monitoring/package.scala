package co.topl.brambl

import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.io.process
import fs2.io.process.Process

package object monitoring {

  def runProcess(command: String, args: Seq[String]): Resource[IO, Process[IO]] =
    process.ProcessBuilder(command, args.toList).spawn[IO]

  def connectBifrostNodes(networkName: String, nodeName: String): Resource[IO, Process[IO]] =
    runProcess("docker", Seq("network", "connect", networkName, nodeName))

  def disconnectBifrostNodes(networkName: String, nodeName: String): Resource[IO, Process[IO]] =
    runProcess("docker", Seq("network", "disconnect", networkName, nodeName))

}
