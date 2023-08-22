package co.topl.brambl.syntax

import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances.groupPolicyEventImmutable
import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.models.GroupId
import com.google.protobuf.ByteString
import java.security.MessageDigest
import scala.language.implicitConversions

trait GroupPolicySyntax {

  implicit def groupPolicyAsGroupPolicySyntaxOps(groupPolicy: GroupPolicy): GroupPolicyAsGroupPolicySyntaxOps =
    new GroupPolicyAsGroupPolicySyntaxOps(groupPolicy)
}

class GroupPolicyAsGroupPolicySyntaxOps(val groupPolicy: GroupPolicy) extends AnyVal {

  def computeId: GroupId = {
    val digest: Array[Byte] = groupPolicy.immutable.value.toByteArray
    val sha256 = MessageDigest.getInstance("SHA-256").digest(digest)
    GroupId(ByteString.copyFrom(sha256))
  }
}
