package co.topl.brambl.display

import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.models.{Datum, GroupId, SeriesId}
import co.topl.brambl.utils.Encoding
import co.topl.brambl.models.box.Value

trait GroupDisplayOps {
  implicit val groupIdDisplay: DisplayOps[GroupId] = (id: GroupId) => Encoding.encodeToHex(id.value.toByteArray())

  implicit val groupPolicyDisplay: DisplayOps[Datum.GroupPolicy] = (gp: Datum.GroupPolicy) => s"""
Label: ${gp.event.label}
Regitration-Utxo: ${gp.event.registrationUtxo.display}
Fixed-Series: ${displayFixedSeries(gp.event.fixedSeries)}
    """

  implicit val groupDisplay: DisplayOps[Value.Group] = (group: Value.Group) =>
    s"Group Constructor\n" +
    s"Id           : ${group.groupId.display}\n" +
    s"Fixed-Series : ${displayFixedSeries(group.fixedSeries)}"

  private def displayFixedSeries(fixedSeries: Option[SeriesId]): String =
    fixedSeries.map(sId => sId.display).getOrElse("NO FIXED SERIES")
}
