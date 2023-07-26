package co.topl.brambl.dataApi

/**
 * @param yIdx The Y coordinate associated with the contract
 * @param name The name of the contract
 * @param lockTemplate The lock template associated with the contract
 */
case class WalletContract(yIdx: Int, name: String, lockTemplate: String)

/**
 * Defines a contract storage API.
 */
trait ContractStorageAlgebra[F[_]] {

  /**
   * Fetches all contracts.
   * @return The fetched contracts.
   */
  def findContracts(): F[Seq[WalletContract]]

  /**
   * Add a new contract.
   * @param walletContract The wallet contract to add.
   */
  def addContract(walletContract: WalletContract): F[Int]
}

object ContractStorageAlgebra {

  /**
   * @param yIdx         The Y coordinate associated with the contract
   * @param name         The name of the contract
   * @param lockTemplate The lock template associated with the contract
   */
  case class WalletContract(yIdx: Int, name: String, lockTemplate: String)
}
