package co.topl.brambl.builders.locks

import cats.Id
import quivr.models.Proposition.Value._
import co.topl.brambl.MockHelpers
import co.topl.brambl.builders.locks.PropositionTemplate.UnableToBuildPropositionTemplate
import co.topl.brambl.models.box.Lock.Value.Predicate

class LockTemplateSpec extends munit.FunSuite with MockHelpers {

  test("Build Predicate Lock via Template") {
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
      List(notTemplate, orTemplate),
      2
    )
    val lockTemplate = LockTemplate.PredicateTemplate[Id](List(andTemplate, thresholdTemplate), 2)

    val lockInstance = lockTemplate.build(mockVks)
    assert(lockInstance.isRight)
    val lockPredicate = lockInstance.toOption.get
    assert(lockPredicate.value.isPredicate)
    val andProposition = lockPredicate.value.asInstanceOf[Predicate].value.challenges.head.getRevealed
    assert(andProposition.value.isAnd)
    val andLeftProposition = andProposition.value.asInstanceOf[And].value.left
    val andRightProposition = andProposition.value.asInstanceOf[And].value.right
    assert(andLeftProposition.value.isDigitalSignature)
    assert(andRightProposition.value.isDigitalSignature)
    assertEquals(andLeftProposition.value.asInstanceOf[DigitalSignature].value.routine, MockSigningRoutine)
    assertEquals(andLeftProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, andLeftEntityVk)
    assertEquals(andRightProposition.value.asInstanceOf[DigitalSignature].value.routine, MockSigningRoutine)
    assertEquals(andRightProposition.value.asInstanceOf[DigitalSignature].value.verificationKey, andRightEntityVk)

    val thresholdProposition = lockPredicate.value.asInstanceOf[Predicate].value.challenges(1).getRevealed
    assert(thresholdProposition.value.isThreshold)
    val notProposition = thresholdProposition.value.asInstanceOf[Threshold].value.challenges.head
    assert(notProposition.value.isNot)
    val innerProposition = notProposition.value.asInstanceOf[Not].value.proposition
    assert(innerProposition.value.isHeightRange)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.chain, MockChain)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.min, MockMin)
    assertEquals(innerProposition.value.asInstanceOf[HeightRange].value.max, MockMax)
    val orProposition = thresholdProposition.value.asInstanceOf[Threshold].value.challenges(1)
    assert(orProposition.value.isOr)
    val orLeftProposition = orProposition.value.asInstanceOf[Or].value.left
    val orRightProposition = orProposition.value.asInstanceOf[Or].value.right
    assert(orLeftProposition.value.isLocked)
    assertEquals(orLeftProposition.value.asInstanceOf[Locked].value.data, None)
    assert(orRightProposition.value.isTickRange)
    assertEquals(orRightProposition.value.asInstanceOf[TickRange].value.min, MockMin)
    assertEquals(orRightProposition.value.asInstanceOf[TickRange].value.max, MockMax)
  }

  test("Failure to build Predicate Lock via Template > Invalid Entity Index") {
    val andLeftSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](MockSigningRoutine, 0)
    val andRightSignatureTemplate = PropositionTemplate.SignatureTemplate[Id](MockSigningRoutine, 5)
    val andTemplate = PropositionTemplate.AndTemplate[Id](andLeftSignatureTemplate, andRightSignatureTemplate)
    val heightTemplate = PropositionTemplate.HeightTemplate[Id](MockChain, MockMin, MockMax)
    val notTemplate = PropositionTemplate.NotTemplate[Id](heightTemplate)
    val lockedTemplate = PropositionTemplate.LockedTemplate[Id](None)
    val tickTemplate = PropositionTemplate.TickTemplate[Id](MockMin, MockMax)
    val orTemplate = PropositionTemplate.OrTemplate[Id](lockedTemplate, tickTemplate)
    val thresholdTemplate = PropositionTemplate.ThresholdTemplate[Id](List(notTemplate, orTemplate), 2)
    val lockTemplate = LockTemplate.PredicateTemplate[Id](List(andTemplate, thresholdTemplate), 2)

    val lockInstance = lockTemplate.build(mockVks)
    assert(lockInstance.isLeft)
    assert(lockInstance.swap.toOption.get.isInstanceOf[UnableToBuildPropositionTemplate])
  }
}
