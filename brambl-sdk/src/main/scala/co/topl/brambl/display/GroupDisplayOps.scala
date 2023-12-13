package co.topl.brambl.display

import co.topl.brambl.models.GroupId
import co.topl.brambl.utils.Encoding

trait GroupDisplayOps {
  implicit val groupIdDisplay: DisplayOps[GroupId] = (id: GroupId) => Encoding.encodeToHex(id.value.toByteArray())
}
