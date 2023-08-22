package co.topl.brambl.syntax

import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances.{groupPolicyEventImmutable, groupValueImmutable}
import co.topl.brambl.models.GroupId
import co.topl.brambl.models.box.Value.Group
import com.google.protobuf.ByteString
import java.security.MessageDigest
import scala.language.implicitConversions

trait GroupSyntax {

  implicit def groupAsGroupSyntaxOps(group: Group): GroupAsGroupSyntaxOps =
    new GroupAsGroupSyntaxOps(group)
}

class GroupAsGroupSyntaxOps(val group: Group) extends AnyVal {

  /**
   * The ID of this group.  If an ID was pre-computed and saved in the Group, it is restored.
   * Otherwise, a new ID is computed (but not saved in the Group).
   */
  def id: GroupId =
    group.groupId.getOrElse(computeId)

  /**
   * Computes what the ID _should_ be for this Group.
   */
  def computeId: GroupId = group.groupPolicy.computeId

  /**
   * Compute a new ID and return a copy of this Group with the new ID embedded.
   * Any previous value will be overwritten in the new copy.
   */
  def embedId: Group =
    group.copy(groupId = Some(computeId))

  /**
   * Returns true if this Group contains a valid embedded ID.
   */
  def containsValidId: Boolean =
    group.groupId.contains(computeId)
}
