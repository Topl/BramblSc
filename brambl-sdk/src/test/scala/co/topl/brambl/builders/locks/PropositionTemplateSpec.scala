package co.topl.brambl.builders.locks

import cats.Id
import co.topl.brambl.MockHelpers
import co.topl.brambl.builders.locks.PropositionTemplate.UnableToBuildPropositionTemplate
import com.google.protobuf.ByteString
import quivr.models.Data
import quivr.models.Proposition.Value._

class PropositionTemplateSpec extends munit.FunSuite with MockHelpers {

  test("Build Locked Proposition via Template") {
    val data = Some(Data(ByteString.copyFrom("someData".getBytes)))
    val lockedTemplate = PropositionTemplate.LockedTemplate[Id](data)
    val lockedInstance = lockedTemplate.build(mockVks)
    assert(lockedInstance.isRight)
    val lockedProposition = lockedInstance.toOption.get
    assert(lockedProposition.value.isLocked)
    assertEquals(lockedProposition.value.asInstanceOf[Locked].value.data, data)
  }

  test("Build Height Proposition via Template") {
    val heightTemplate = PropositionTemplate.HeightTemplate[Id](MockChain, MockMin, MockMax)
    val heightInstance = heightTemplate.build(mockVks)
    assert(heightInstance.isRight)
    val heightProposition = heightInstance.toOption.get
    assert(heightProposition.value.isHeightRange)
    assertEquals(heightProposition.value.asInstanceOf[HeightRange].value.chain, MockChain)
    assertEquals(heightProposition.value.asInstanceOf[HeightRange].value.min, MockMin)
    assertEquals(heightProposition.value.asInstanceOf[HeightRange].value.max, MockMax)
  }

  test("Build Tick Proposition via Template") {
    val tickTemplate = PropositionTemplate.TickTemplate[Id](MockMin, MockMax)
    val tickInstance = tickTemplate.build(mockVks)
    assert(tickInstance.isRight)
    val tickProposition = tickInstance.toOption.get
    assert(tickProposition.value.isTickRange)
    assertEquals(tickProposition.value.asInstanceOf[TickRange].value.min, MockMin)
    assertEquals(tickProposition.value.asInstanceOf[TickRange].value.max, MockMax)
  }

  test("Build Digest Proposition via Template") {
    val digestTemplate = PropositionTemplate.DigestTemplate[Id](MockDigestRoutine, MockDigest)
    val digestInstance = digestTemplate.build(mockVks)
    assert(digestInstance.isRight)
    val digestProposition = digestInstance.toOption.get
    assert(digestProposition.value.isDigest)
    assertEquals(digestProposition.value.asInstanceOf[Digest].value.routine, MockDigestRoutine)
    assertEquals(digestProposition.value.asInstanceOf[Digest].value.digest, MockDigest)
  }

  test("Build Sha256 Digest Proposition via Template") {
    val digestTemplate = PropositionTemplate.DigestTemplate[Id](MockSha256DigestRoutine, MockSha256Digest)
    // No verification keys needed for digest. However, supplying them should not affect the result.
    val digestInstance = digestTemplate.build(Nil)
    assert(digestInstance.isRight)
    val digestProposition = digestInstance.toOption.get
    assert(digestProposition.value.isDigest)
    assertEquals(digestProposition.value.asInstanceOf[Digest].value.routine, "Sha256")
    assertEquals(digestProposition.value.asInstanceOf[Digest].value.digest, MockSha256Digest)
  }

  test("Build Signature Proposition via Template") {
    val entityIdx = 0
    val entityVk = mockVks(entityIdx)
    val signatureTemplate = PropositionTemplate.SignatureTemplate[Id](MockSigningRoutine, entityIdx)
    val signatureInstance = signatureTemplate.build(mockVks)
    assert(signatureInstance.isRight)
    val signatureProposition = signatureInstance.toOption.get
    assert(signatureProposition.value.isDigitalSignature)
    assertEquals(signatureProposition.value.asInstanceOf[DigitalSignature].value.routine, MockSigningRoutine)
    assertEquals(signatureProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, entityVk)
  }

  test("Failure to Build Signature Proposition via Template > Invalid Entity Index") {
    val entityIdx = 2
    val signatureTemplate = PropositionTemplate.SignatureTemplate[Id](MockSigningRoutine, entityIdx)
    val signatureInstance = signatureTemplate.build(mockVks)
    assert(signatureInstance.isLeft)
    assert(signatureInstance.swap.toOption.get.isInstanceOf[UnableToBuildPropositionTemplate])
  }

  test("Build And Proposition via Template") {
    val leftEntityIdx = 0
    val rightEntityIdx = 1
    val leftEntityVk = mockVks(leftEntityIdx)
    val rightEntityVk = mockVks(rightEntityIdx)
    val leftSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](MockSigningRoutine, leftEntityIdx)
    val rightSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](MockSigningRoutine, rightEntityIdx)
    val andTemplate = PropositionTemplate.AndTemplate[Id](leftSignatureTemplate, rightSignatureTemplate)
    val andInstance = andTemplate.build(mockVks)
    assert(andInstance.isRight)
    val andProposition = andInstance.toOption.get
    assert(andProposition.value.isAnd)
    val leftProposition = andProposition.value.asInstanceOf[And].value.left
    val rightProposition = andProposition.value.asInstanceOf[And].value.right
    assert(leftProposition.value.isDigitalSignature)
    assert(rightProposition.value.isDigitalSignature)
    assertEquals(leftProposition.value.asInstanceOf[DigitalSignature].value.routine, MockSigningRoutine)
    assertEquals(leftProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, leftEntityVk)
    assertEquals(rightProposition.value.asInstanceOf[DigitalSignature].value.routine, MockSigningRoutine)
    assertEquals(rightProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, rightEntityVk)
  }

  test("Build Or Proposition via Template") {
    val leftEntityIdx = 0
    val rightEntityIdx = 1
    val leftEntityVk = mockVks(leftEntityIdx)
    val rightEntityVk = mockVks(rightEntityIdx)
    val leftSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](MockSigningRoutine, leftEntityIdx)
    val rightSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](MockSigningRoutine, rightEntityIdx)
    val orTemplate = PropositionTemplate.OrTemplate[Id](leftSignatureTemplate, rightSignatureTemplate)
    val orInstance = orTemplate.build(mockVks)
    assert(orInstance.isRight)
    val orProposition = orInstance.toOption.get
    assert(orProposition.value.isOr)
    val leftProposition = orProposition.value.asInstanceOf[Or].value.left
    val rightProposition = orProposition.value.asInstanceOf[Or].value.right
    assert(leftProposition.value.isDigitalSignature)
    assert(rightProposition.value.isDigitalSignature)
    assertEquals(leftProposition.value.asInstanceOf[DigitalSignature].value.routine, MockSigningRoutine)
    assertEquals(leftProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, leftEntityVk)
    assertEquals(rightProposition.value.asInstanceOf[DigitalSignature].value.routine, MockSigningRoutine)
    assertEquals(rightProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, rightEntityVk)
  }

  test("Build Not Proposition via Template") {
    val heightTemplate = PropositionTemplate.HeightTemplate[Id](MockChain, MockMin, MockMax)
    val notTemplate = PropositionTemplate.NotTemplate[Id](heightTemplate)
    val notInstance = notTemplate.build(mockVks)
    assert(notInstance.isRight)
    val notProposition = notInstance.toOption.get
    assert(notProposition.value.isNot)
    val innerProposition = notProposition.value.asInstanceOf[Not].value.proposition
    assert(innerProposition.value.isHeightRange)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.chain, MockChain)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.min, MockMin)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.max, MockMax)
  }

  test("Build Threshold Proposition via Template") {
    val andLeftEntityIdx = 0
    val andRightEntityIdx = 1
    val andLeftEntityVk = mockVks(andLeftEntityIdx)
    val andRightEntityVk = mockVks(andRightEntityIdx)
    val andLeftSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](MockSigningRoutine, andLeftEntityIdx)
    val andRightSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](MockSigningRoutine, andRightEntityIdx)
    val andTemplate = PropositionTemplate.AndTemplate[Id](andLeftSignatureTemplate, andRightSignatureTemplate)
    val heightTemplate = PropositionTemplate.HeightTemplate[Id](MockChain, MockMin, MockMax)
    val notTemplate = PropositionTemplate.NotTemplate[Id](heightTemplate)
    val lockedTemplate = PropositionTemplate.LockedTemplate[Id](None)
    val tickTemplate = PropositionTemplate.TickTemplate[Id](MockMin, MockMax)
    val orTemplate = PropositionTemplate.OrTemplate[Id](lockedTemplate, tickTemplate)
    val thresholdTemplate = PropositionTemplate.ThresholdTemplate[Id](
      List(andTemplate, notTemplate, orTemplate),
      3
    )

    val thresholdInstance = thresholdTemplate.build(mockVks)
    assert(thresholdInstance.isRight)
    val thresholdProposition = thresholdInstance.toOption.get
    assert(thresholdProposition.value.isThreshold)
    val andProposition = thresholdProposition.value.asInstanceOf[Threshold].value.challenges.head
    assert(andProposition.value.isAnd)
    val andLeftProposition = andProposition.value.asInstanceOf[And].value.left
    val andRightProposition = andProposition.value.asInstanceOf[And].value.right
    assert(andLeftProposition.value.isDigitalSignature)
    assert(andRightProposition.value.isDigitalSignature)
    assertEquals(andLeftProposition.value.asInstanceOf[DigitalSignature].value.routine, MockSigningRoutine)
    assertEquals(andLeftProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, andLeftEntityVk)
    assertEquals(andRightProposition.value.asInstanceOf[DigitalSignature].value.routine, MockSigningRoutine)
    assertEquals(andRightProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, andRightEntityVk)
    val notProposition = thresholdProposition.value.asInstanceOf[Threshold].value.challenges(1)
    assert(notProposition.value.isNot)
    val innerProposition = notProposition.value.asInstanceOf[Not].value.proposition
    assert(innerProposition.value.isHeightRange)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.chain, MockChain)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.min, MockMin)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.max, MockMax)
    val orProposition = thresholdProposition.value.asInstanceOf[Threshold].value.challenges(2)
    assert(orProposition.value.isOr)
    val orLeftProposition = orProposition.value.asInstanceOf[Or].value.left
    val orRightProposition = orProposition.value.asInstanceOf[Or].value.right
    assert(orLeftProposition.value.isLocked)
    assertEquals(orLeftProposition.value.asInstanceOf[Locked].value.data, None)
    assert(orRightProposition.value.isTickRange)
    assertEquals(orRightProposition.value.asInstanceOf[TickRange].value.min, MockMin)
    assertEquals(orRightProposition.value.asInstanceOf[TickRange].value.max, MockMax)
  }
}
