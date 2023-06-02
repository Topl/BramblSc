package co.topl.brambl.wallet

import cats.data.ValidatedNel
import co.topl.brambl.builders.locks.LockTemplate
import co.topl.brambl.models.Indices
import co.topl.brambl.models.box.Lock
import quivr.models.{Preimage, Proposition, VerificationKey}

abstract class WalletStateApiFailure extends RuntimeException

trait WalletStateAlgebra[F[_]] {

  /**
   * Initialize the wallet state with the given verification key
   *
   * @param vk The verification key to initialize the wallet state with
   */
  def initWalletState(vk: VerificationKey): F[Unit]

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
   * Get the current address for the wallet state
   *
   * @return The current address of the wallet state as a string in base58 encoding
   */
  def getCurrentAddress: F[String]

  /**
   * Update the wallet state with a new set of Predicate Lock, Lock Address, and their associated Indices
   *
   * @param lockPredicate The lock predicate to add to the wallet state
   * @param lockAddress    The lock address to add to the wallet state
   * @param routine        The routine to add to the wallet state
   * @param vk             The verification key to add to the wallet state
   * @param indices        The indices to add to the wallet state
   */
  def updateWalletState(
    lockPredicate: String,
    lockAddress:   String,
    routine:       Option[String],
    vk:            Option[String],
    indices:       Indices
  ): F[Unit]

  /**
   * Get the current indices for the given party, contract and optional state
   *
   * @param party   A String label of the party to get the indices for
   * @param contract A String label of the contract to get the indices for
   * @param someState The optional state index of the indices. If not provided, the next state index for the given party
   *                  and contract pair will be used
   * @return The indices for the given party, contract and optional state if possible. Else None
   */
  def getCurrentIndicesForFunds(party: String, contract: String, someState: Option[Int]): F[Option[Indices]]

  /**
   * Validate that the supplied party, contract and optional state exist and are associated with each other in the
   * current wallet state
   *
   * @param party   A String label of the party to validate with
   * @param contract A String label of the contract to validate with
   * @param someState The optional state index to validate with. If not provided, the next state for the given party
   *                  and contract pair will be used
   * @return The indices for the given party, contract and optional state if valid. If not, the relevant errors
   */
  def validateCurrentIndicesForFunds(
    party:     String,
    contract:  String,
    someState: Option[Int]
  ): F[ValidatedNel[String, Indices]]

  /**
   * Get the next available indices for the given party and contract
   *
   * @param party   A String label of the party to get the next indices for
   * @param contract A String label of the contract to get the next indices for
   * @return The next indices for the given party and contract if possible. Else None
   */
  def getNextIndicesForFunds(party: String, contract: String): F[Option[Indices]]

  /**
   * Get the lock predicate associated to the given indices
   *
   * @param indices The indices to get the lock predicate for
   * @return The lock predicate for the given indices if possible. Else None
   */
  def getLockByIndex(indices: Indices): F[Option[Lock.Predicate]]

  /**
   * Get the lock address associated to the given party, contract and optional state
   *
   * @param party   A String label of the party to get the lock address for
   * @param contract A String label of the contract to get the lock address for
   * @param someState The optional state index to get the lock address for. If not provided, the next state for the
   *                  given party and contract pair will be used
   * @return The lock address for the given indices if possible. Else None
   */
  def getAddress(party: String, contract: String, someState: Option[Int]): F[Option[String]]

  /**
   * Add a new entry of entity verification keys to the wallet state's cartesian indexing. Entities are at a pair of
   * x (party) and y (contract) layers and thus represent a Child verification key at a participants own x/y path.
   * The respective x and y indices of the specified party and contract labels must already exist.
   *
   * @param party   A String label of the party to associate the new verification keys with
   * @param contract A String label of the contract to associate the new verification keys with
   * @param entities The list of Verification Keys in base58 format to add
   */
  def addEntityVks(party: String, contract: String, entities: List[String]): F[Unit]

  /**
   * Get the list of verification keys associated to the given pair of party and contract
   *
   * @param party   A String label of the party to get the verification keys for
   * @return The list of verification keys in base58 format associated to the given party and contract if possible.
   *         Else None. It is possible that the list of entities is empty.
   */
  def getEntityVks(party: String, contract: String): F[Option[List[String]]]

  /**
   * Add a new lock template entry to the wallet state's cartesian indexing. Lock templates are at the y (contract)
   * layer. This new entry will be associated to the label given by contract. The index of the new entry (and thus
   * associated with the contract label) will be automatically derived by the next available y-index.
   *
   * @param contract   A String label of the contract to associate the new lockTemplate entry with
   * @param lockTemplate The list of Lock Templates of the lock templates to add to the new Entries entry
   */
  def addNewLockTemplate(contract: String, lockTemplate: LockTemplate[F]): F[Unit]

  /**
   * Get the lock template associated to the given contract
   *
   * @param contract A String label of the contract to get the lock template for
   * @return The lock template associated to the given contract if possible. Else None.
   */
  def getLockTemplate(contract: String): F[Option[LockTemplate[F]]]

  /**
   * Using the template associated the given contract, the verification keys associated to the party and contract pair,
   * and the z state given by nextState, build a Lock
   *
   * @param party A String label of the party to get the Lock verification keys for
   * @param contract A String label of the contract to get the verification keys and template for
   * @param nextState The z index state to build the lock for
   * @return A built lock, if possible. Else none
   */
  def getLock(party: String, contract: String, nextState: Int): F[Option[Lock]]
}
