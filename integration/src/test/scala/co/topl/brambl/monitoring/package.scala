package co.topl.brambl

import cats.effect.IO
import fs2.io.process
import fs2.text

package object monitoring {
  private def getText(p: fs2.io.process.Process[IO]): IO[String] =
    p.stdout
      .through(text.utf8.decode)
      .compile
      .foldMonoid
      .map(_.trim)
  private def runProcess(args: Seq[String]): IO[String] =
    process.ProcessBuilder(args.head, args.tail.toList).spawn[IO].use(getText)

  def startDockerContainer(container: String): IO[Unit] = runProcess(
    Seq("docker", "start", container)
  ).void

  def stopDockerContainer(container: String): IO[Unit] = runProcess(
    Seq("docker", "stop", container)
  ).void

  def restartDockerContainer(container: String): IO[Unit] = runProcess(
    Seq("docker", "restart", container)
  ).void

  def getConfig(container: String): IO[String] = runProcess(
    Seq("docker", "exec", container, "cat", "/bifrost-staking/config.yaml")
  )

  def updateConfig(container: String, newConfig: String): IO[Unit] = runProcess(
    Seq("docker", "exec", container, "sh", "-c", "\"echo", newConfig, ">", "/bifrost-staking/config.yaml\"")
  ).void

  def addKnownPeer(config: String, ip: String): String = if(config.contains("p2p")) config else config + s"  p2p:\n    known-peers: $ip:9085"
  def removeKnownPeer(config: String): String = if(config.contains("p2p")) config.substring(0, config.indexOf("p2p")) else config

  def getIpAddr(container: String): IO[String] = runProcess(
    Seq("docker", "inspect", container, "--format", "{{.NetworkSettings.IPAddress}}")
  )

  def connectBifrostNodes(node: String, otherNode: String): IO[Unit] = for {
    oldConfig <- getConfig(node)
    _ <- IO.println(oldConfig)
    ip <- getIpAddr(otherNode)
    newConfig = addKnownPeer(oldConfig, ip)
    _ <- IO.println(newConfig)
    _ <- updateConfig(node, newConfig)
//    _ <- stopDockerContainer(otherNode)
    _ <- restartDockerContainer(node)
//    _ <- startDockerContainer(otherNode)
  } yield ()

  def disconnectBifrostNodes(node: String): IO[Unit] = for {
    oldConfig <- getConfig(node)
    _ <- IO.println(oldConfig)

    newConfig = removeKnownPeer(oldConfig)
    _ <- IO.println(newConfig)

    _ <- updateConfig(node, newConfig)
    //    _ <- stopDockerContainer(otherNode)
    _ <- restartDockerContainer(node)
    //    _ <- startDockerContainer(otherNode)
  } yield ()

  def isRunning(container: String): IO[Boolean] = runProcess(
    Seq("docker", "inspect", container, "--format", "{{.State.Running}}")
  ).map(_.equals("true"))

}
