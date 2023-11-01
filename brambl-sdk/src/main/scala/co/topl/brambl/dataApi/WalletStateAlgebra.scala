package co.topl.brambl.dataApi

import cats.data.ValidatedNel
import co.topl.brambl.builders.locks.LockTemplate
import co.topl.brambl.models.Indices
import co.topl.brambl.models.box.Lock
import quivr.models.{Preimage, Proposition, VerificationKey}

/**
 * Defines a data API for storing and retrieving wallet interaction.
 */
trait WalletStateAlgebra[F[_]] {

  /**
   * Initialize the wallet interaction with the given verification key
   *
   * @param networkId The network id to initialize the wallet interaction with
   * @param ledgerId The ledger id to initialize the wallet interaction with
   * @param vk The verification key to initialize the wallet interaction with
   */
  def initWalletState(networkId: Int, ledgerId: Int, vk: VerificationKey): F[Unit]

  /**
   * Get the indices associated to a signature proposition
   *
   * @param signatureProposition The signature proposition to get the indices for
   * @return The indices associated to the signature proposition if it exists. Else None
   */
  def getIndicesBySignature(signatureProposition: Proposition.DigitalSignature): F[Option[Indices]]

  /**
   * Get the preimage secret associated to a digest proposition.
   *
   * @param digestProposition The Digest Proposition for which to retrieve the preimage secret for
   * @return The preimage secret associated to the Digest Proposition if it exists. Else None
   */
  def getPreimage(digestProposition: Proposition.Digest): F[Option[Preimage]]

  /**
   * Get the current address for the wallet interaction
   *
   * @return The current address of the wallet interaction as a string in base58 encoding
   */
  def getCurrentAddress: F[String]

  /**
   * Update the wallet interaction with a new set of Predicate Lock, Lock Address, and their associated Indices
   *
   * @param lockPredicate The lock predicate to add to the wallet interaction
   * @param lockAddress    The lock address to add to the wallet interaction
   * @param routine        The routine to add to the wallet interaction
   * @param vk             The verification key to add to the wallet interaction
   * @param indices        The indices to add to the wallet interaction
   */
  def updateWalletState(
    lockPredicate: String,
    lockAddress:   String,
    routine:       Option[String],
    vk:            Option[String],
    indices:       Indices
  ): F[Unit]

  /**
   * Get the current indices for the given fellowship, template and optional interaction
   *
   * @param fellowship   A String label of the fellowship to get the indices for
   * @param template A String label of the template to get the indices for
   * @param someInteraction The optional interaction index of the indices. If not provided, the next interaction index for the given fellowship
   *                  and template pair will be used
   * @return The indices for the given fellowship, template and optional interaction if possible. Else None
   */
  def getCurrentIndicesForFunds(fellowship: String, template: String, someInteraction: Option[Int]): F[Option[Indices]]

  /**
   * Set the current interaction for the given fellowship and template.
   * In practice, this will remove all interactions after the given interaction index
   * from the database, as the current interaction is the latest interaction.
   * The interaction needs to be smaller or equal than the current interaction.
   *
   * @param fellowship  A String label of the fellowship to set the current interaction for
   * @param template A String label of the template to set the current interaction for
   * @param interaction The interaction index to set the current interaction to
   * @return The indices for the given fellowship, template and interaction. If the interaction is not valid, None.
   */
  def setCurrentIndices(fellowship: String, template: String, interaction: Int): F[Option[Indices]]

  /**
   * Validate that the supplied fellowship, template and optional interaction exist and are associated with each other in the
   * current wallet interaction
   *
   * @param fellowship   A String label of the fellowship to validate with
   * @param template A String label of the template to validate with
   * @param someInteraction The optional interaction index to validate with. If not provided, the next interaction for the given fellowship
   *                  and template pair will be used
   * @return The indices for the given fellowship, template and optional interaction if valid. If not, the relevant errors
   */
  def validateCurrentIndicesForFunds(
    fellowship:      String,
    template:        String,
    someInteraction: Option[Int]
  ): F[ValidatedNel[String, Indices]]

  /**
   * Get the next available indices for the given fellowship and template
   *
   * @param fellowship   A String label of the fellowship to get the next indices for
   * @param template A String label of the template to get the next indices for
   * @return The next indices for the given fellowship and template if possible. Else None
   */
  def getNextIndicesForFunds(fellowship: String, template: String): F[Option[Indices]]

  /**
   * Get the lock predicate associated to the given indices
   *
   * @param indices The indices to get the lock predicate for
   * @return The lock predicate for the given indices if possible. Else None
   */
  def getLockByIndex(indices: Indices): F[Option[Lock.Predicate]]

  /**
   * Get the lock predicate associated to the given lockAddress.
   *
   * @param lockAddress The lockAddress for which we are retrieving the lock for
   * @return The lock predicate for the lockAddress if possible. Else None
   */
  def getLockByAddress(lockAddress: String): F[Option[Lock.Predicate]]

  /**
   * Get the lock address associated to the given fellowship, template and optional interaction
   *
   * @param fellowship   A String label of the fellowship to get the lock address for
   * @param template A String label of the template to get the lock address for
   * @param someInteraction The optional interaction index to get the lock address for. If not provided, the next interaction for the
   *                  given fellowship and template pair will be used
   * @return The lock address for the given indices if possible. Else None
   */
  def getAddress(fellowship: String, template: String, someInteraction: Option[Int]): F[Option[String]]

  /**
   * Add a new entry of fellow verification keys to the wallet interaction's cartesian indexing. Entities are at a pair of
   * x (fellowship) and y (template) layers and thus represent a Child verification key at a participants own x/y path.
   * The respective x and y indices of the specified fellowship and template labels must already exist.
   *
   * @param fellowship   A String label of the fellowship to associate the new verification keys with
   * @param template A String label of the template to associate the new verification keys with
   * @param fellows The list of Verification Keys in base58 format to add
   */
  def addEntityVks(fellowship: String, template: String, fellows: List[String]): F[Unit]

  /**
   * Get the list of verification keys associated to the given pair of fellowship and template
   *
   * @param fellowship   A String label of the fellowship to get the verification keys for
   * @return The list of verification keys in base58 format associated to the given fellowship and template if possible.
   *         Else None. It is possible that the list of fellows is empty.
   */
  def getEntityVks(fellowship: String, template: String): F[Option[List[String]]]

  /**
   * Add a new lock template entry to the wallet interaction's cartesian indexing. Lock templates are at the y (template)
   * layer. This new entry will be associated to the label given by template. The index of the new entry (and thus
   * associated with the template label) will be automatically derived by the next available y-index.
   *
   * @param template   A String label of the template to associate the new lockTemplate entry with
   * @param lockTemplate The list of Lock Templates of the lock templates to add to the new Entries entry
   */
  def addNewLockTemplate(template: String, lockTemplate: LockTemplate[F]): F[Unit]

  /**
   * Get the lock template associated to the given template
   *
   * @param template A String label of the template to get the lock template for
   * @return The lock template associated to the given template if possible. Else None.
   */
  def getLockTemplate(template: String): F[Option[LockTemplate[F]]]

  /**
   * Using the template associated the given template, the verification keys associated to the fellowship and template pair,
   * and the z interaction given by nextInteraction, build a Lock
   *
   * @param fellowship A String label of the fellowship to get the Lock verification keys for
   * @param template A String label of the template to get the verification keys and template for
   * @param nextInteraction The z index interaction to build the lock for
   * @return A built lock, if possible. Else none
   */
  def getLock(fellowship: String, template: String, nextInteraction: Int): F[Option[Lock]]
}
