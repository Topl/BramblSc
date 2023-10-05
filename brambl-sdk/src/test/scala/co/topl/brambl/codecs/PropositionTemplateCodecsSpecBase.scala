package co.topl.brambl.codecs

import cats.Id
import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.MockHelpers
import co.topl.brambl.builders.locks.LockTemplate.PredicateTemplate
import co.topl.brambl.builders.locks.PropositionTemplate.{
  AndTemplate,
  DigestTemplate,
  HeightTemplate,
  LockedTemplate,
  NotTemplate,
  OrTemplate,
  SignatureTemplate,
  ThresholdTemplate,
  TickTemplate
}
import co.topl.brambl.utils.Encoding.encodeToBase58
import com.google.protobuf.ByteString
import io.circe.Json
import quivr.models.Data

trait PropositionTemplateCodecsSpecBase extends MockHelpers {

  object ExpectedLockedProposition {
    val data: Array[Byte] = "Hello world".getBytes
    val value: LockedTemplate[Id] = LockedTemplate[Id](Data(ByteString.copyFrom(data)).some)

    val fields: List[(String, Json)] = List(
      "type" -> Json.fromString(value.propositionType.label),
      "data" -> Json.fromString(encodeToBase58(data))
    )
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedHeightProposition {
    val value: HeightTemplate[Id] = HeightTemplate[Id](MockChain, MockMin, MockMax)

    val fields: List[(String, Json)] = List(
      "type"  -> Json.fromString(value.propositionType.label),
      "chain" -> Json.fromString(MockChain),
      "min"   -> Json.fromLong(MockMin),
      "max"   -> Json.fromLong(MockMax)
    )
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedTickProposition {
    val value: TickTemplate[Id] = TickTemplate[Id](MockMin, MockMax)

    val fields: List[(String, Json)] = List(
      "type" -> Json.fromString(value.propositionType.label),
      "min"  -> Json.fromLong(MockMin),
      "max"  -> Json.fromLong(MockMax)
    )
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedDigestProposition {
    val value: DigestTemplate[Id] = DigestTemplate[Id](MockDigestRoutine, MockDigest)

    val fields: List[(String, Json)] = List(
      "type"    -> Json.fromString(value.propositionType.label),
      "routine" -> Json.fromString(MockDigestRoutine),
      "digest"  -> Json.fromString(encodeToBase58(MockDigest.value.toByteArray))
    )
    val json: Json = Json.fromFields(fields)
  }

  object ExpectedSignatureProposition {
    def value(entityIdx: Int): SignatureTemplate[Id] = SignatureTemplate[Id](MockSigningRoutine, entityIdx)

    def fields(entityIdx: Int): List[(String, Json)] = List(
      "type"      -> Json.fromString(value(entityIdx).propositionType.label),
      "routine"   -> Json.fromString(MockSigningRoutine),
      "entityIdx" -> Json.fromInt(entityIdx)
    )

    def json(entityIdx: Int): Json = Json.fromFields(fields(entityIdx))
  }

  object ExpectedAndProposition {

    val value: AndTemplate[Id] = AndTemplate[Id](
      ExpectedSignatureProposition.value(0),
      ExpectedSignatureProposition.value(1)
    )

    val fields: List[(String, Json)] = List(
      "type"  -> Json.fromString(value.propositionType.label),
      "left"  -> ExpectedSignatureProposition.json(0),
      "right" -> ExpectedSignatureProposition.json(1)
    )

    val json: Json = Json.fromFields(fields)
  }

  object ExpectedOrProposition {

    val value: OrTemplate[Id] = OrTemplate[Id](
      ExpectedLockedProposition.value,
      ExpectedTickProposition.value
    )

    val fields: List[(String, Json)] = List(
      "type"  -> Json.fromString(value.propositionType.label),
      "left"  -> ExpectedLockedProposition.json,
      "right" -> ExpectedTickProposition.json
    )

    val json: Json = Json.fromFields(fields)
  }

  object ExpectedNotProposition {
    val value: NotTemplate[Id] = NotTemplate[Id](ExpectedHeightProposition.value)

    val fields: List[(String, Json)] = List(
      "type"          -> Json.fromString(value.propositionType.label),
      "innerTemplate" -> ExpectedHeightProposition.json
    )

    val json: Json = Json.fromFields(fields)
  }

  object ExpectedThresholdProposition {

    val value: ThresholdTemplate[Id] = ThresholdTemplate[Id](
      List(
        ExpectedAndProposition.value,
        ExpectedOrProposition.value,
        ExpectedNotProposition.value
      ),
      3
    )

    val fields: List[(String, Json)] = List(
      "type"      -> Json.fromString(value.propositionType.label),
      "threshold" -> Json.fromInt(value.threshold),
      "innerTemplates" -> Json.fromValues(
        List(
          ExpectedAndProposition.json,
          ExpectedOrProposition.json,
          ExpectedNotProposition.json
        )
      )
    )

    val json: Json = Json.fromFields(fields)
  }

  object ExpectedPredicateLock {

    val value: PredicateTemplate[Id] = PredicateTemplate[Id](
      List(
        ExpectedAndProposition.value,
        ExpectedThresholdProposition.value
      ),
      2
    )

    val fields: List[(String, Json)] = List(
      "type"      -> Json.fromString(value.lockType.label),
      "threshold" -> Json.fromInt(value.threshold),
      "innerTemplates" -> Json.fromValues(
        List(
          ExpectedAndProposition.json,
          ExpectedThresholdProposition.json
        )
      )
    )

    val json: Json = Json.fromFields(fields)
  }

}
