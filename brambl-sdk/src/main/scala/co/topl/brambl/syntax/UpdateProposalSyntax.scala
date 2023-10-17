package co.topl.brambl.syntax

import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances.updateProposalImmutable
import co.topl.brambl.models.UpdateProposalId
import co.topl.brambl.models.box.Value.UpdateProposal
import com.google.protobuf.ByteString
import java.security.MessageDigest
import scala.language.implicitConversions

trait UpdateProposalSyntax {

  implicit def updateProposalAsUpdateProposalSyntaxOps(
    updateProposal: UpdateProposal
  ): UpdateProposalAsUpdateProposalSyntaxOps =
    new UpdateProposalAsUpdateProposalSyntaxOps(updateProposal)
}

class UpdateProposalAsUpdateProposalSyntaxOps(val updateProposal: UpdateProposal) extends AnyVal {

  def computeId: UpdateProposalId = {
    val digest: Array[Byte] = updateProposal.immutable.value.toByteArray
    val sha256 = MessageDigest.getInstance("SHA-256").digest(digest)
    UpdateProposalId(ByteString.copyFrom(sha256))
  }
}
