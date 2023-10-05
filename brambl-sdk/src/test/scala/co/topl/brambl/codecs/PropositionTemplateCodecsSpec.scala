package co.topl.brambl.codecs

import cats.Id
import co.topl.brambl.builders.locks.PropositionTemplate
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
import co.topl.brambl.codecs.PropositionTemplateCodecs.{decodePropositionTemplate, encodePropositionTemplate}
import io.circe.Json

class PropositionTemplateCodecsSpec extends munit.FunSuite with PropositionTemplateCodecsSpecBase {

  def assertEncodeDecode[TemplateType <: PropositionTemplate[Id]](
    expectedValue: TemplateType,
    expectedJson:  Json
  ): Unit = {
    // Decode test
    val testPropositionRes = decodePropositionTemplate[Id](expectedJson)
    assert(testPropositionRes.isRight)
    assert(testPropositionRes.toOption.get.isInstanceOf[TemplateType])
    val templateInstance = testPropositionRes.toOption.get.asInstanceOf[TemplateType]
    assertEquals(templateInstance, expectedValue)

    // Encode test
    val testJson = encodePropositionTemplate(expectedValue)
    assert(!testJson.isNull)
    assertEquals(testJson, expectedJson)

    // Decode then Encode test
    val encodedFromDecoded = encodePropositionTemplate(templateInstance)
    assert(!encodedFromDecoded.isNull)
    assertEquals(encodedFromDecoded, expectedJson)

    // Encode then Decode test
    val decodedFromEncoded = decodePropositionTemplate[Id](testJson)
    assert(decodedFromEncoded.isRight)
    assert(decodedFromEncoded.toOption.get.isInstanceOf[TemplateType])
    val templateInstanceFromEncoded = testPropositionRes.toOption.get.asInstanceOf[TemplateType]
    assertEquals(templateInstanceFromEncoded, expectedValue)
  }

  test("Encode and Decode Locked Proposition Template") {
    assertEncodeDecode[LockedTemplate[Id]](ExpectedLockedProposition.value, ExpectedLockedProposition.json)
  }

  test("Encode and Decode Height Proposition Template") {
    assertEncodeDecode[HeightTemplate[Id]](ExpectedHeightProposition.value, ExpectedHeightProposition.json)
  }

  test("Encode and Decode Tick Proposition Template") {
    assertEncodeDecode[TickTemplate[Id]](ExpectedTickProposition.value, ExpectedTickProposition.json)
  }

  test("Encode and Decode Digest Proposition Template") {
    assertEncodeDecode[DigestTemplate[Id]](ExpectedDigestProposition.value, ExpectedDigestProposition.json)
  }

  test("Encode and Decode Signature Proposition Template") {
    assertEncodeDecode[SignatureTemplate[Id]](
      ExpectedSignatureProposition.value(0),
      ExpectedSignatureProposition.json(0)
    )
  }

  test("Encode and Decode And Proposition Template") {
    assertEncodeDecode[AndTemplate[Id]](ExpectedAndProposition.value, ExpectedAndProposition.json)
  }

  test("Encode and Decode Or Proposition Template") {
    assertEncodeDecode[OrTemplate[Id]](ExpectedOrProposition.value, ExpectedOrProposition.json)
  }

  test("Encode and Decode Not Proposition Template") {
    assertEncodeDecode[NotTemplate[Id]](ExpectedNotProposition.value, ExpectedNotProposition.json)
  }

  test("Encode and Decode Threshold Proposition Template") {
    assertEncodeDecode[ThresholdTemplate[Id]](ExpectedThresholdProposition.value, ExpectedThresholdProposition.json)
  }

}
