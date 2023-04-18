package co.topl.brambl.dataApi

import co.topl.brambl.dataApi.DataApi._
import co.topl.brambl.models.{Indices, LockAddress, TransactionOutputAddress}
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.crypto.encryption.VaultStore
import quivr.models.Preimage

/**
 * Defines a storage API for fetching and storing keys and states.
 *
 * TEMPORARY: This is a temporary interface that will be replaced by a more robust interface in the future. It currently
 * only includes functionality that is needed for the current implementation of other components (Credentialler and
 * builders).
 *
 * TODO: Design and replace this interface with the actual interface that will be used by the rest of the system.
 */
trait DataApi[F[_]] {

  /**
   * Return the indices associated to a TransactionOutputAddress.
   *
   * Simplifying assumption *for now* is that TransactionOutputAddress and Indices are 1 to 1. This assumption may
   * change once more work is done to define the Cartesian Indexing scheme.
   *
   * TODO: Revisit this assumption once the Cartesian Indexing scheme is more fleshed out.
   *
   * @param address The TransactionOutputAddress for which to retrieve the indices
   * @return The indices associated to the known identifier if it exists. Else None
   */
  def getIndicesByTxoAddress(address: TransactionOutputAddress): Option[Indices]

  /**
   * Return the UTXO targeted by a TransactionOutputAddress.
   *
   * A TransactionOutputAddress identifies an output (UTXO) of an existing transaction on the chain.
   *
   * @param address The TransactionOutputAddress of the UTXO to retrieve
   * @return The UTXO targeted by the given address, if it exists. Else None
   */
  def getUtxoByTxoAddress(address: TransactionOutputAddress): Option[UnspentTransactionOutput]

  /**
   * Return the Lock targeted by a LockAddress
   *
   * A LockAddress is meant to identify a Lock on chain.
   *
   * @param address The LockAddress for which to retrieve the Lock
   * @return The Lock targeted by the given address, if it exists. Else None
   */
  def getLockByLockAddress(address: LockAddress): Option[Lock]

  /**
   * Return the preimage secret associated to indices.
   *
   * @param idx The indices for which to retrieve the preimage secret
   * @return The preimage secret associated to the indices if it exists. Else None
   */
  def getPreimage(idx: Indices): Option[Preimage]

  /**
   * Persist a VaultStore for the Topl Main Secret Key.
   *
   * @param mainKeyVaultStore The VaultStore to persist
   * @param name              The name identifier of the VaultStore. This is used to manage multiple wallet identities.
   *                          Most commonly, only one wallet identity will be used. It is the responsibility of the dApp
   *                          to manage the names of the wallet identities if multiple will be used.
   * @return nothing if successful. If persisting fails due to an underlying cause, return a DataApiException
   */
  def saveMainKeyVaultStore(mainKeyVaultStore: VaultStore[F], name: String): F[Either[DataApiException, Unit]]

  /**
   * Return the VaultStore for the Topl Main Secret Key.
   *
   * @param name The name identifier  of the VaultStore. This is used to manage multiple wallet identities.
   *             Most commonly, only one wallet identity will be used. It is the responsibility of the dApp to manage
   *             the names of the wallet identities if multiple will be used.
   * @return The VaultStore for the Topl Main Secret Key if it exists. If retrieving fails due to an underlying cause, return a DataApiException
   */
  def getMainKeyVaultStore(name: String): F[Either[DataApiException, VaultStore[F]]]
}

object DataApi {
  abstract class DataApiException(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)
}
