package co.topl.brambl.dataApi

/**
 * @param yIdx The Y coordinate associated with the contract
 * @param name The name of the contract
 * @param lockTemplate The lock template associated with the contract
 */
case class WalletTemplate(yIdx: Int, name: String, lockTemplate: String)

/**
 * Defines a contract storage API.
 */
trait TemplateStorageAlgebra[F[_]] {

  /**
   * Fetches all templates.
   * @return The fetched templates.
   */
  def findTemplates(): F[Seq[WalletTemplate]]

  /**
   * Add a new contract.
   * @param walletTemplate The wallet contract to add.
   */
  def addTemplate(walletTemplate: WalletTemplate): F[Int]
}
