package co.topl.brambl.dataApi

import co.topl.brambl.dataApi.DataApi._
import co.topl.brambl.models.{Evidence, Indices, LockAddress, TransactionOutputAddress}
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.crypto.encryption.VaultStore
import quivr.models.{Preimage, Proposition}

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
   * Return the UTXO targeted by a TransactionOutputAddress.
   *
   * A TransactionOutputAddress identifies an output (UTXO) of an existing transaction on the chain.
   *
   * @param address The TransactionOutputAddress of the UTXO to retrieve
   * @return The UTXO targeted by the given address, if it exists. Else a DataApiException
   */
  def getUtxoByTxoAddress(address: TransactionOutputAddress): F[Either[DataApiException, UnspentTransactionOutput]]

  /**
   * Return the Lock targeted by a LockAddress.
   *
   * A LockAddress is meant to identify a Lock on chain.
   *
   * @param address The LockAddress for which to retrieve the Lock
   * @return The Lock targeted by the given address, if it exists. Else a DataApiException
   */
  def getLockByLockAddress(address: LockAddress): F[Either[DataApiException, Lock]]

  /**
   * Return the preimage secret associated to a digest proposition.
   *
   * @param digestProposition The Digest Proposition for which to retrieve the preimage secret for
   * @return The preimage secret associated to the Digest Proposition if it exists. Else a DataApiException
   */
  def getPreimage(digestProposition: Proposition.Digest): F[Either[DataApiException, Preimage]]

  /**
   * Return the indices (x/y/z) associated to a signature proposition. A Signature Proposition is created with a
   * verification and must be signed with the corresponding signing key. The verification and signing key pair is
   * derived from the indices.
   *
   * @param signatureProposition The Signature Proposition for which to retrieve the indices for
   * @return The indices associated to the Signature Proposition if it exists. Else a DataApiException
   */
  def getIndices(signatureProposition: Proposition.DigitalSignature): F[Either[DataApiException, Indices]]

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

  /**
   * Update a persisted VaultStore for the Topl Main Secret Key.
   *
   * @param name              The name identifier of the VaultStore to update. This is used to manage multiple wallet identities.
   *                          Most commonly, only one wallet identity will be used. It is the responsibility of the dApp
   *                          to manage the names of the wallet identities if multiple will be used.
   * @return nothing if successful. If the update fails due to an underlying cause (for ex does not exist), return a DataApiException
   */
  def updateMainKeyVaultStore(mainKeyVaultStore: VaultStore[F], name: String): F[Either[DataApiException, Unit]]

  /**
   * Delete a persisted VaultStore for the Topl Main Secret Key.
   *
   * @param name The name identifier of the VaultStore to delete. This is used to manage multiple wallet identities.
   *             Most commonly, only one wallet identity will be used. It is the responsibility of the dApp
   *             to manage the names of the wallet identities if multiple will be used.
   * @return nothing if successful. If the deletion fails due to an underlying cause (for ex does not exist), return a DataApiException
   */
  def deleteMainKeyVaultStore(name: String): F[Either[DataApiException, Unit]]
}

object DataApi {
  abstract class DataApiException(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)
}
