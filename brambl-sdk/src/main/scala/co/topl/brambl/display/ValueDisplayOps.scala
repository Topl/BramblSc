package co.topl.brambl.display

import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.Value.{Asset, Group, Lvl, Series, Topl}
import co.topl.brambl.syntax.{int128AsBigInt, valueToQuantitySyntaxOps}
import co.topl.brambl.utils.Encoding

import scala.util.{Failure, Success, Try}

trait ValueDisplayOps {

  implicit val valueDisplay: DisplayOps[Value.Value] = (value: Value.Value) =>
    s"""${typeDisplay(value)}\n${quantityDisplay(value)}"""

  implicit val groupDisplay: DisplayOps[Value.Group] = (group: Value.Group) =>
    s"Group Constructor\n" +
    s"Id           : ${group.groupId.display}\n" +
    s"Fixed-Series : ${group.fixedSeries.map(sId => sId.display).getOrElse("NO FIXED SERIES")}"

  implicit val seriesDisplay: DisplayOps[Value.Series] = (series: Value.Series) =>
    s"Series Constructor\n" +
    s"Id           : ${series.seriesId.display}\n" +
    s"Fungibility  : ${series.fungibility.display}\n" +
    s"Token-Supply : ${series.tokenSupply.getOrElse("UNLIMITED")}\n" +
    s"Quant-Descr. : ${series.quantityDescriptor.display}"

  implicit val assetDisplay: DisplayOps[Value.Asset] = (asset: Value.Asset) =>
    s"Asset\n" +
    s"GroupId      : ${asset.groupId.map(gId => gId.display).getOrElse("N/A")}\n" +
    s"SeriesId     : ${asset.seriesId.map(sId => sId.display).getOrElse("N/A")}\n" +
    s"Commitment   : ${asset.commitment
        .map(x => Encoding.encodeToHex(x.toByteArray()))
        .getOrElse("No commitment")}\n" +
    s"Ephemeral-Metadata: \n" +
    s"${asset.ephemeralMetadata.map(meta => meta.display).getOrElse("No ephemeral metadata")}"

  def typeDisplay(value: Value.Value): String = {
    val vType = value match {
      case Lvl(_)    => "LVL"
      case Group(g)  => g.display
      case Series(s) => s.display
      case Asset(a)  => a.display
      case Topl(_)   => "TOPL"
      case _         => "Unknown txo type"
    }
    "Type         : " + vType
  }

  def quantityDisplay(value: Value.Value): String = {
    val quantity = Try {
      value.quantity
    } match {
      case Success(asInt128) => (asInt128: BigInt).toString()
      case Failure(_)        => "Undefine type"
    }
    "Value      : " + quantity
  }
}
