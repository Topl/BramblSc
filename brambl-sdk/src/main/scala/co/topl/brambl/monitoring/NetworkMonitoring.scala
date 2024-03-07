package co.topl.brambl.monitoring

object NetworkMonitoring {
  // replace string with BlockId
  def initializeBitcoinMonitor[F[_]](blockId: Option[String] = None): F[BitcoinMonitor] = ???
}
