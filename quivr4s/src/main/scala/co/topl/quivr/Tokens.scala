package co.topl.quivr

/**
 * See spec at https://github.com/Topl/protobuf-specs/blob/main/quivr/models/proof.proto
 */
object Tokens {

  val Locked = "locked"
  val Digest = "digest"
  val DigitalSignature = "digital_signature"
  val HeightRange = "height_range"
  val TickRange = "tick_range"
  val ExactMatch = "exact_match"
  val LessThan = "less_than"
  val GreaterThan = "greater_than"
  val EqualTo = "equal_to"
  val Threshold = "threshold"
  val Not = "not"
  val And = "and"
  val Or = "or"

}
