package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.builders.TransactionBuilderApi.UnableToBuildTransaction
import co.topl.brambl.models.box.QuantityDescriptorType.{ACCUMULATOR, FRACTIONABLE, IMMUTABLE}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.{
  assetAsBoxVal,
  bigIntAsInt128,
  groupAsBoxVal,
  groupPolicyAsGroupPolicySyntaxOps,
  int128AsBigInt,
  ioTransactionAsTransactionSyntaxOps,
  seriesAsBoxVal,
  seriesPolicyAsSeriesPolicySyntaxOps,
  valueToQuantitySyntaxOps,
  GroupAndSeriesFungible,
  GroupFungible,
  SeriesFungible
}

// TODO: Implementation TBD
class TransactionBuilderInterpreterTransferAllSpec extends TransactionBuilderInterpreterSpecBase {

  // Test user validations
  // Test transfer all of each type (group, series, lvl, liquid assets (x3 for all fungibility), accumulator assets)
  // test transfer of ALL everything

  test("buildTransferAllTransaction > tbd") {
    assertEquals(true, true)
  }

}
