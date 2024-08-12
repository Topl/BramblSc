package co.topl.brambl.builders

class TransactionBuilderInterpreterAssetMergingSpec extends TransactionBuilderInterpreterSpecBase {
  test("Invalid user params > a UTXO in UTXOs to merge does not exist in TXOs") {
  }
  test("Invalid user params > UTXOs to merge are not compatible") {
  }
  test("Invalid user params > a TXO does not have a corresponding lock") {
  }
  test("Invalid user params > a lock does not have a corresponding TXO") {
  }
  test("Invalid user params > insufficient funds for fees") {
  }
  test("Fee edge case (exact funds, no change)") {
  }
  test("Generic case") {
    // Check ASM to see if its valid; contains everythign mentioned in utxosToMerge, outputIDX points to the right utxo
    // Check merged output (correct lock address, ephemeral metadata, commitment, merkleroot, IDs, etc
    // check change; goes to right changeAddress, correct amount
    // check the ability to split (construct UTXOs and verify)
  }
}
