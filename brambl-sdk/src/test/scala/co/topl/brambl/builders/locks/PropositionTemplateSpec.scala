package co.topl.brambl.builders.locks

import cats.Id
import co.topl.brambl.MockHelpers
import co.topl.brambl.builders.locks.PropositionTemplate.UnableToBuildPropositionTemplate
import com.google.protobuf.ByteString
import quivr.models.Proposition.Value._
import quivr.models.Data

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
    val chain = "someChain"
    val min = 0L
    val max = 100L
    val heightTemplate = PropositionTemplate.HeightTemplate[Id](chain, min, max)
    val heightInstance = heightTemplate.build(mockVks)
    assert(heightInstance.isRight)
    val heightProposition = heightInstance.toOption.get
    assert(heightProposition.value.isHeightRange)
    assertEquals(heightProposition.value.asInstanceOf[HeightRange].value.chain, chain)
    assertEquals(heightProposition.value.asInstanceOf[HeightRange].value.min, min)
    assertEquals(heightProposition.value.asInstanceOf[HeightRange].value.max, max)
  }

  test("Build Tick Proposition via Template") {
    val min = 0L
    val max = 100L
    val tickTemplate = PropositionTemplate.TickTemplate[Id](min, max)
    val tickInstance = tickTemplate.build(mockVks)
    assert(tickInstance.isRight)
    val tickProposition = tickInstance.toOption.get
    assert(tickProposition.value.isTickRange)
    assertEquals(tickProposition.value.asInstanceOf[TickRange].value.min, min)
    assertEquals(tickProposition.value.asInstanceOf[TickRange].value.max, max)
  }

  test("Build Digest Proposition via Template") {
    val routine = "someRoutine"
    val digestTemplate = PropositionTemplate.DigestTemplate[Id](routine, MockDigest)
    val digestInstance = digestTemplate.build(mockVks)
    assert(digestInstance.isRight)
    val digestProposition = digestInstance.toOption.get
    assert(digestProposition.value.isDigest)
    assertEquals(digestProposition.value.asInstanceOf[Digest].value.routine, routine)
    assertEquals(digestProposition.value.asInstanceOf[Digest].value.digest, MockDigest)
  }

  test("Build Signature Proposition via Template") {
    val routine = "someRoutine"
    val entityIdx = 0
    val entityVk = mockVks(entityIdx)
    val signatureTemplate = PropositionTemplate.SignatureTemplate[Id](routine, entityIdx)
    val signatureInstance = signatureTemplate.build(mockVks)
    assert(signatureInstance.isRight)
    val signatureProposition = signatureInstance.toOption.get
    assert(signatureProposition.value.isDigitalSignature)
    assertEquals(signatureProposition.value.asInstanceOf[DigitalSignature].value.routine, routine)
    assertEquals(signatureProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, entityVk)
  }

  test("Failure to Build Signature Proposition via Template > Invalid Entity Index") {
    val entityIdx = 2
    val signatureTemplate = PropositionTemplate.SignatureTemplate[Id]("someRoutine", entityIdx)
    val signatureInstance = signatureTemplate.build(mockVks)
    assert(signatureInstance.isLeft)
    assert(signatureInstance.swap.toOption.get.isInstanceOf[UnableToBuildPropositionTemplate])
  }

  test("Build And Proposition via Template") {
    val routine = "someRoutine"
    val leftEntityIdx = 0
    val rightEntityIdx = 1
    val leftEntityVk = mockVks(leftEntityIdx)
    val rightEntityVk = mockVks(rightEntityIdx)
    val leftSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](routine, leftEntityIdx)
    val rightSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](routine, rightEntityIdx)
    val andTemplate = PropositionTemplate.AndTemplate[Id](leftSignatureTemplate, rightSignatureTemplate)
    val andInstance = andTemplate.build(mockVks)
    assert(andInstance.isRight)
    val andProposition = andInstance.toOption.get
    assert(andProposition.value.isAnd)
    val leftProposition = andProposition.value.asInstanceOf[And].value.left
    val rightProposition = andProposition.value.asInstanceOf[And].value.right
    assert(leftProposition.value.isDigitalSignature)
    assert(rightProposition.value.isDigitalSignature)
    assertEquals(leftProposition.value.asInstanceOf[DigitalSignature].value.routine, routine)
    assertEquals(leftProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, leftEntityVk)
    assertEquals(rightProposition.value.asInstanceOf[DigitalSignature].value.routine, routine)
    assertEquals(rightProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, rightEntityVk)
  }

  test("Build Or Proposition via Template") {
    val routine = "someRoutine"
    val leftEntityIdx = 0
    val rightEntityIdx = 1
    val leftEntityVk = mockVks(leftEntityIdx)
    val rightEntityVk = mockVks(rightEntityIdx)
    val leftSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](routine, leftEntityIdx)
    val rightSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](routine, rightEntityIdx)
    val orTemplate = PropositionTemplate.OrTemplate[Id](leftSignatureTemplate, rightSignatureTemplate)
    val orInstance = orTemplate.build(mockVks)
    assert(orInstance.isRight)
    val orProposition = orInstance.toOption.get
    assert(orProposition.value.isOr)
    val leftProposition = orProposition.value.asInstanceOf[Or].value.left
    val rightProposition = orProposition.value.asInstanceOf[Or].value.right
    assert(leftProposition.value.isDigitalSignature)
    assert(rightProposition.value.isDigitalSignature)
    assertEquals(leftProposition.value.asInstanceOf[DigitalSignature].value.routine, routine)
    assertEquals(leftProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, leftEntityVk)
    assertEquals(rightProposition.value.asInstanceOf[DigitalSignature].value.routine, routine)
    assertEquals(rightProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, rightEntityVk)
  }

  test("Build Not Proposition via Template") {
    val chain = "someChain"
    val min = 0L
    val max = 100L
    val heightTemplate = PropositionTemplate.HeightTemplate[Id](chain, min, max)
    val notTemplate = PropositionTemplate.NotTemplate[Id](heightTemplate)
    val notInstance = notTemplate.build(mockVks)
    assert(notInstance.isRight)
    val notProposition = notInstance.toOption.get
    assert(notProposition.value.isNot)
    val innerProposition = notProposition.value.asInstanceOf[Not].value.proposition
    assert(innerProposition.value.isHeightRange)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.chain, chain)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.min, min)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.max, max)
  }

  test("Build Threshold Proposition via Template") {
    val routine = "someRoutine"
    val andLeftEntityIdx = 0
    val andRightEntityIdx = 1
    val andLeftEntityVk = mockVks(andLeftEntityIdx)
    val andRightEntityVk = mockVks(andRightEntityIdx)
    val andLeftSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](routine, andLeftEntityIdx)
    val andRightSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](routine, andRightEntityIdx)
    val andTemplate = PropositionTemplate.AndTemplate[Id](andLeftSignatureTemplate, andRightSignatureTemplate)
    val chain = "someChain"
    val min = 0L
    val max = 100L
    val heightTemplate = PropositionTemplate.HeightTemplate[Id](chain, min, max)
    val notTemplate = PropositionTemplate.NotTemplate[Id](heightTemplate)
    val lockedTemplate = PropositionTemplate.LockedTemplate[Id](None)
    val tickTemplate = PropositionTemplate.TickTemplate[Id](min, max)
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
    assertEquals(andLeftProposition.value.asInstanceOf[DigitalSignature].value.routine, routine)
    assertEquals(andLeftProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, andLeftEntityVk)
    assertEquals(andRightProposition.value.asInstanceOf[DigitalSignature].value.routine, routine)
    assertEquals(andRightProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, andRightEntityVk)
    val notProposition = thresholdProposition.value.asInstanceOf[Threshold].value.challenges(1)
    assert(notProposition.value.isNot)
    val innerProposition = notProposition.value.asInstanceOf[Not].value.proposition
    assert(innerProposition.value.isHeightRange)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.chain, chain)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.min, min)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.max, max)
    val orProposition = thresholdProposition.value.asInstanceOf[Threshold].value.challenges(2)
    assert(orProposition.value.isOr)
    val orLeftProposition = orProposition.value.asInstanceOf[Or].value.left
    val orRightProposition = orProposition.value.asInstanceOf[Or].value.right
    assert(orLeftProposition.value.isLocked)
    assertEquals(orLeftProposition.value.asInstanceOf[Locked].value.data, None)
    assert(orRightProposition.value.isTickRange)
    assertEquals(orRightProposition.value.asInstanceOf[TickRange].value.min, min)
    assertEquals(orRightProposition.value.asInstanceOf[TickRange].value.max, max)
  }
}
