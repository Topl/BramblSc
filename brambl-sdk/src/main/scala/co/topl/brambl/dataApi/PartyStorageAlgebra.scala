package co.topl.brambl.dataApi

/**
 * @param xIdx The X coordinate associated with the entity
 * @param name The name of the entity
 */
case class WalletEntity(xIdx: Int, name: String)

/**
 * Defines a party storage API.
 */
trait PartyStorageAlgebra[F[_]] {

  /**
   * Fetches all parties.
   * @return The fetched parties.
   */
  def findParties(): F[Seq[WalletEntity]]

  /**
   * Add a new party.
   * @param walletEntity The wallet entity to add.
   */
  def addParty(walletEntity: WalletEntity): F[Int]
}
