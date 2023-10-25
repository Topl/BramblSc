package co.topl.brambl.syntax

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.{Value => BoxValue}
import com.google.protobuf.ByteString

class TokenTypeIdentifierSyntaxSpec extends munit.FunSuite with MockHelpers {

  test("typeIdentifier") {
    val gId = mockGroupPolicy.computeId
    val sId = mockSeriesPolicy.computeId
    val qd = mockSeriesPolicy.quantityDescriptor
    assertEquals(lvlValue.value.typeIdentifier, LvlType)
    assertEquals(groupValue.value.typeIdentifier, GroupType(gId))
    assertEquals(seriesValue.value.typeIdentifier, SeriesType(sId))
    assertEquals(assetGroupSeries.value.typeIdentifier, AssetType(gId.value, sId.value))
    assertEquals(assetGroup.value.typeIdentifier, AssetType(gId.value, sId.value))
    assertEquals(assetSeries.value.typeIdentifier, AssetType(gId.value, sId.value))
    val mockAlloy = ByteString.copyFrom(Array.fill(32)(0.toByte))
    val testAlloy = ByteString.copyFrom(Array.fill(32)(0.toByte))
    assertEquals(
      assetGroup.copy(assetGroup.getAsset.copy(seriesAlloy = mockAlloy.some)).value.typeIdentifier,
      AssetType(gId.value, testAlloy)
    )
    assertEquals(
      assetSeries.copy(assetSeries.getAsset.copy(groupAlloy = mockAlloy.some)).value.typeIdentifier,
      AssetType(testAlloy, sId.value)
    )
    intercept[Exception](BoxValue.Topl(Value.TOPL(quantity)).typeIdentifier)
  }
}
