package co.topl.brambl.syntax

import cats.Monoid
import co.topl.brambl.models.box.Value.Asset
import com.google.protobuf.ByteString
import co.topl.brambl.common.MonoidInstances.instances._
import scala.language.implicitConversions

trait AssetIdSyntax {

  implicit def assetIdAsAssetIdSyntaxOps(asset: Asset): AssetIdSyntaxOps =
    new AssetIdSyntaxOps(asset)
}

class AssetIdSyntaxOps(val a: Asset) extends AnyVal {

  /**
   * The groupId+seriesId this Asset. groupId+seriesId
   */
  def id: ByteString =
    Monoid.combine(a.groupId.map(_.value), a.seriesId.map(_.value)).getOrElse(ByteString.EMPTY)

}
