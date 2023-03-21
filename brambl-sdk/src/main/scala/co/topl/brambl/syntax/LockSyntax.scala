package co.topl.brambl.syntax

import co.topl.brambl.common.ContainsEvidence
import co.topl.brambl.common.ContainsImmutable.instances.lockImmutable
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.box.Lock

import scala.language.implicitConversions

trait LockSyntax {

  implicit def lockAsLockSyntaxOps(lock: Lock): LockSyntaxOps =
    new LockSyntaxOps(lock)

  implicit def predicateLockAsLockSyntaxOps(lock: Lock.Predicate): PredicateLockSyntaxOps =
    new PredicateLockSyntaxOps(lock)
}

class LockSyntaxOps(val lock: Lock) extends AnyVal {

  def address(network: Int, ledger: Int): LockAddress =
    LockAddress(network, ledger)
      .withLock32(
        Identifier.Lock32(
          ContainsEvidence[Lock].sized32Evidence(lock)
        )
      )
}

class PredicateLockSyntaxOps(val lock: Lock.Predicate) extends AnyVal {

  def address(network: Int, ledger: Int): LockAddress =
    LockAddress(network, ledger)
      .withLock32(
        Identifier.Lock32(
          ContainsEvidence[Lock].sized32Evidence(Lock().withPredicate(lock))
        )
      )
}
