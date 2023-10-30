package co.topl.brambl.dataApi

/**
 * @param xIdx The X coordinate associated with the entity
 * @param name The name of the entity
 */
case class WalletFellowship(xIdx: Int, name: String)

/**
 * Defines a fellowship storage API.
 */
trait FellowshipStorageAlgebra[F[_]] {

  /**
   * Fetches all fellowships.
   * @return The fetched fellowships.
   */
  def findFellowships(): F[Seq[WalletFellowship]]

  /**
   * Add a new fellowship.
   * @param walletEntity The wallet entity to add.
   */
  def addFellowship(walletEntity: WalletFellowship): F[Int]
}
