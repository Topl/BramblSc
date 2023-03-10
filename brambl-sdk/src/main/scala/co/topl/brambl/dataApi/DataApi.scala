package co.topl.brambl.dataApi

import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box.Box
import co.topl.brambl.models.Indices
import co.topl.brambl.routines.signatures.Signing
import quivr.models.{KeyPair, Preimage}

/**
 * Defines a storage API for fetching and storing keys and states.
 *
 * TEMPORARY: This is a temporary interface that will be replaced by a more robust interface in the future. It currently
 * only includes functionality that is needed for the current implementation of other components (Credentialler and
 * builders).
 *
 * TODO: Design and replace this interface with the actual interface that will be used by the rest of the system.
 */
trait DataApi {

  /**
   * Return the indices associated to a known identifier.
   * Simplifying assumption is that KnownIdentifier and Indices are 1 to 1
   *
   * @param id The known identifier for which to retrieve the indices
   * @return The indices associated to the known identifier if it exists. Else None
   */
  def getIndicesByKnownIdentifier(id: TransactionOutputAddress): Option[Indices]

  /**
   * Return the box associated to a known identifier.
   *
   * A Box is created from a utxo. A KnownIdentifier combines an Identifier and an index. For the simple use-case,
   * we are only considering the KnownIdentifiers that are already defined in our ecosystem; TransactionOutput32 and
   * TransactionOutput64, both of which refer to a transaction output (i.e, utxo).
   *
   * Therefore, we can make the simplifying assumption that Box and KnownIdentifier are 1 to 1
   *
   * @param id The known identifier for which to retrieve the box
   * @return The box associated to the known identifier if it exists. Else None
   */
  def getBoxByKnownIdentifier(id: TransactionOutputAddress): Option[Box]

  /**
   * Return the preimage secret associated to indices.
   *
   * @param idx The indices for which to retrieve the preimage secret
   * @return The preimage secret associated to the indices if it exists. Else None
   */
  def getPreimage(idx: Indices): Option[Preimage]

  /**
   * Return the key pair associated to indices.
   *
   * @param idx     The indices for which to retrieve the key pair
   * @param routine The signing routine to use to generate the key pair
   * @return The key pair associated to the indices if it exists. Else None
   */
  def getKeyPair(idx: Indices, routine: Signing): Option[KeyPair]
}
