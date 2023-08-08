package co.topl

package object quivr {

  // The operations offered via the Quivr DSL
  object Operations {
    trait Locked

    trait Digest

    trait DigitalSignature

    trait HeightRange

    trait TickRange

    trait MustInclude

    trait ExactMatch

    trait LessThan

    trait GreaterThan

    trait EqualTo

    trait Threshold

    trait Not

    trait And

    trait Or
  }

}
