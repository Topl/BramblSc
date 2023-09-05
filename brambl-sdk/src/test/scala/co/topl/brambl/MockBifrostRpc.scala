package co.topl.brambl

import cats.effect.IO
import co.topl.brambl.dataApi.BifrostQueryAlgebra
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.consensus.models.BlockId
import co.topl.node.models.BlockBody

object MockBifrostRpc extends BifrostQueryAlgebra[IO] with MockHelpers {

  val lockAddressToLock: Map[TransactionId, IoTransaction] = Map(
    txFull.computeId            -> txFull,
    txFullAlternative.computeId -> txFullAlternative
  )

  override def fetchTransaction(txId: TransactionId): IO[Option[IoTransaction]] =
    IO.pure(lockAddressToLock.get(txId))

  // The following are not implemented since they are not used in the tests
  override def blockByHeight(height: Long): IO[Option[(BlockId, BlockBody, Seq[IoTransaction])]] = ???

  override def blockById(blockId: BlockId): IO[Option[(BlockId, BlockBody, Seq[IoTransaction])]] = ???
}
