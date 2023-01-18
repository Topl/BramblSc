package co.topl.brambl.common

object Tags {
  /**
   * See spec at https://github.com/Topl/protobuf-specs/blob/main/brambl/models/identifier.proto
   */
  object Identifier {
    val Lock32 = "box_lock_32"
    val Lock64 = "box_lock_64"
    val BoxValue32 = "box_value_32"
    val BoxValue64 = "box_value_64"
    val IoTransaction32 = "io_transaction_32"
    val IoTransaction64 = "io_transaction_64"
    val AccumulatorRoot32 = "acc_root_32"
    val AccumulatorRoot64 = "acc_root_64"
  }
}