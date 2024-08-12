package co.topl.brambl.builders

class MergingOpsSpec extends TransactionBuilderInterpreterSpecBase {
  test("Valid Merge Compatibility > ") {

  }
  test("Invalid Merge Compatibility > only 1 TXO provided") {
  }
  test("Invalid Merge Compatibility > Repeat UTXO address in TXOs") {

  }
  test("Invalid Merge Compatibility > TXOs contain invalid identifier") {

  }
  test("Invalid Merge Compatibility > TXOs contain non-asset identifier") {

  }
  test("Invalid Merge Compatibility > Repeat TypeIdentifier in TXOs") {

  }
  test("Invalid Merge Compatibility > TXOs do not all share same fungibility type") {

  }
  test("Invalid Merge Compatibility > TXOs contain Group_and_Series fungibility") {

  }
  test("Invalid Merge Compatibility > TXOs with Series fungibility do not share SeriesId") {

  }
  test("Invalid Merge Compatibility > TXOs with Group fungibility do not share GroupId") {

  }
  test("Invalid Merge Compatibility > TXOs do not all share same quantity descriptor type") {

  }
  test("Merge > Basic Group fungible") {
  }
  test("Merge > Basic Series fungible") {
  }
  test("Merge > Group fungible w/ an alloy") {
  }
  test("Merge > Series fungible w/ an alloy") {
  }
  test("Merge > Validate ephemeral metadata and commitment does not affect merkle root") {
  }
  test("Merge > Validate lexicographical order > changing orders does not affect merkle root") {
  }
}
