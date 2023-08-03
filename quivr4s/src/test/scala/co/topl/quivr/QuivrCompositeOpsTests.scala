package co.topl.quivr

import cats.{Id, Monad}
import co.topl.quivr.runtime.QuivrRuntimeErrors
import com.google.protobuf.ByteString
import quivr.models.VerificationKey._
import quivr.models._

/**
 * Set of tests for the Quivr Composite Operations.
 */
class QuivrCompositeOpsTests extends munit.FunSuite with MockHelpers {

  import co.topl.quivr.api.Proposer._
  import co.topl.quivr.api.Prover._
  import co.topl.quivr.api.Verifier.instances._

  implicit val applicativeId: Monad[Id] = cats.catsInstancesForId

  test("An and proposition must evaluate to true when both the verification of both proofs is true") {
    val (sk1, vk1) = VerySecureSignatureRoutine.generateKeyPair()
    val (sk2, vk2) = VerySecureSignatureRoutine.generateKeyPair()
    val signatureProposition1 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk1))))
    )
    val signatureProposition2 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk2))))
    )
    val andProposition = andProposer.propose(signatureProposition1, signatureProposition2)
    val signature1 = VerySecureSignatureRoutine.sign(sk1, signableBytes.value.toByteArray)
    val signature2 = VerySecureSignatureRoutine.sign(sk2, signableBytes.value.toByteArray)
    val signatureProverProof1 = signatureProver.prove(Witness(ByteString.copyFrom(signature1)), signableBytes)
    val signatureProverProof2 = signatureProver.prove(Witness(ByteString.copyFrom(signature2)), signableBytes)
    val andProverProof = andProver.prove((signatureProverProof1, signatureProverProof2), signableBytes)
    val result =
      verifierInstance.evaluate(andProposition, andProverProof, dynamicContext(andProposition, andProverProof))
    assertEquals(result.isRight, true)
  }

  test("An and proposition must evaluate to false when one of the proofs evaluates to false") {
    val (sk1, vk1) = VerySecureSignatureRoutine.generateKeyPair()
    val (_, vk2) = VerySecureSignatureRoutine.generateKeyPair()
    val (sk2, _) = VerySecureSignatureRoutine.generateKeyPair()
    val signatureProposition1 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk1))))
    )
    val signatureProposition2 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk2))))
    )
    val andProposition = andProposer.propose(signatureProposition1, signatureProposition2)
    val signature1 = VerySecureSignatureRoutine.sign(sk1, signableBytes.value.toByteArray)
    val signature2 = VerySecureSignatureRoutine.sign(sk2, signableBytes.value.toByteArray)
    val signatureProverProof1 = signatureProver.prove(Witness(ByteString.copyFrom(signature1)), signableBytes)
    val signatureProverProof2 = signatureProver.prove(Witness(ByteString.copyFrom(signature2)), signableBytes)
    val andProverProof = andProver.prove((signatureProverProof1, signatureProverProof2), signableBytes)
    val result =
      verifierInstance.evaluate(andProposition, andProverProof, dynamicContext(andProposition, andProverProof))
    assertEquals(result.isLeft, true)
    assertEquals(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined,
      true
    )
  }

  test("An or proposition must evaluate to true when one of the proofs evaluates to true") {
    val (sk1, vk1) = VerySecureSignatureRoutine.generateKeyPair()
    val (_, vk2) = VerySecureSignatureRoutine.generateKeyPair()
    val (sk2, _) = VerySecureSignatureRoutine.generateKeyPair()
    val signatureProposition1 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk1))))
    )
    val signatureProposition2 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk2))))
    )
    val orProposition = orProposer.propose(signatureProposition1, signatureProposition2)
    val signature1 = VerySecureSignatureRoutine.sign(sk1, signableBytes.value.toByteArray)
    val signature2 = VerySecureSignatureRoutine.sign(sk2, signableBytes.value.toByteArray)
    val signatureProverProof1 = signatureProver.prove(Witness(ByteString.copyFrom(signature1)), signableBytes)
    val signatureProverProof2 = signatureProver.prove(Witness(ByteString.copyFrom(signature2)), signableBytes)
    val orProverProof = orProver.prove((signatureProverProof1, signatureProverProof2), signableBytes)
    val result = verifierInstance.evaluate(orProposition, orProverProof, dynamicContext(orProposition, orProverProof))
    assertEquals(result.isRight, true)
  }

  test("An or proposition must evaluate to false when both proofs evaluate to false") {
    val (_, vk1) = VerySecureSignatureRoutine.generateKeyPair()
    val (sk1, _) = VerySecureSignatureRoutine.generateKeyPair()
    val (_, vk2) = VerySecureSignatureRoutine.generateKeyPair()
    val (sk2, _) = VerySecureSignatureRoutine.generateKeyPair()
    val signatureProposition1 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk1))))
    )
    val signatureProposition2 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk2))))
    )
    val orProposition = orProposer.propose(signatureProposition1, signatureProposition2)
    val signature1 = VerySecureSignatureRoutine.sign(sk1, signableBytes.value.toByteArray)
    val signature2 = VerySecureSignatureRoutine.sign(sk2, signableBytes.value.toByteArray)
    val signatureProverProof1 = signatureProver.prove(Witness(ByteString.copyFrom(signature1)), signableBytes)
    val signatureProverProof2 = signatureProver.prove(Witness(ByteString.copyFrom(signature2)), signableBytes)
    val orProverProof = orProver.prove((signatureProverProof1, signatureProverProof2), signableBytes)
    val result = verifierInstance.evaluate(orProposition, orProverProof, dynamicContext(orProposition, orProverProof))
    assertEquals(result.isLeft, true)
    assertEquals(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined,
      true
    )
  }

  test("A not proposition must evaluate to false when the proof in the parameter is true") {
    val heightProposition = heightProposer.propose("height", 900, 1000)
    val heightProverProof = heightProver.prove((), signableBytes)
    val notProposition = notProposer.propose(heightProposition)
    val notProverProof = notProver.prove(heightProverProof, signableBytes)
    val result =
      verifierInstance.evaluate(notProposition, notProverProof, dynamicContext(notProposition, notProverProof))
    assertEquals(result.isLeft, true)
    assertEquals(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined,
      true
    )
  }

  test("A not proposition must evaluate to true when the proof in the parameter is false") {
    val heightProposition = heightProposer.propose("height", 1, 10)
    val heightProverProof = heightProver.prove((), signableBytes)
    val notProposition = notProposer.propose(heightProposition)
    val notProverProof = notProver.prove(heightProverProof, signableBytes)
    val result =
      verifierInstance.evaluate(notProposition, notProverProof, dynamicContext(notProposition, notProverProof))
    assertEquals(result.isRight, true)
  }

  test("A threshold proposition must evaluate to true when the threshold is passed") {
    val (sk1, vk1) = VerySecureSignatureRoutine.generateKeyPair()
    val (_, vk2) = VerySecureSignatureRoutine.generateKeyPair()
    val (sk2, _) = VerySecureSignatureRoutine.generateKeyPair()
    val (sk3, vk3) = VerySecureSignatureRoutine.generateKeyPair()
    val signatureProposition1 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk1))))
    )
    val signatureProposition2 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk2))))
    )
    val signatureProposition3 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk3))))
    )
    val thresholdProposition =
      thresholdProposer.propose((Set(signatureProposition1, signatureProposition2, signatureProposition3), 2))
    val signature1 = VerySecureSignatureRoutine.sign(sk1, signableBytes.value.toByteArray)
    val signature2 = VerySecureSignatureRoutine.sign(sk2, signableBytes.value.toByteArray)
    val signature3 = VerySecureSignatureRoutine.sign(sk3, signableBytes.value.toByteArray)
    val signatureProverProof1 = signatureProver.prove(Witness(ByteString.copyFrom(signature1)), signableBytes)
    val signatureProverProof2 = signatureProver.prove(Witness(ByteString.copyFrom(signature2)), signableBytes)
    val signatureProverProof3 = signatureProver.prove(Witness(ByteString.copyFrom(signature3)), signableBytes)
    val thresholdProverProof = thresholdProver.prove(
      Set(signatureProverProof1, signatureProverProof2, signatureProverProof3),
      signableBytes
    )
    val result = verifierInstance.evaluate(
      thresholdProposition,
      thresholdProverProof,
      dynamicContext(thresholdProposition, thresholdProverProof)
    )
    assertEquals(result.isRight, true)
  }

  test("A threshold proposition must evaluate to false when the threshold is not passed") {
    val (sk1, vk1) = VerySecureSignatureRoutine.generateKeyPair()
    val (_, vk2) = VerySecureSignatureRoutine.generateKeyPair()
    val (sk2, _) = VerySecureSignatureRoutine.generateKeyPair()
    val (sk3, vk3) = VerySecureSignatureRoutine.generateKeyPair()
    val (_, vk4) = VerySecureSignatureRoutine.generateKeyPair()
    val (_, vk5) = VerySecureSignatureRoutine.generateKeyPair()
    val signatureProposition1 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk1))))
    )
    val signatureProposition2 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk2))))
    )
    val signatureProposition3 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk3))))
    )
    val signatureProposition4 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk4))))
    )
    val signatureProposition5 = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk5))))
    )
    val thresholdProposition = thresholdProposer.propose(
      (
        Set(
          signatureProposition1,
          signatureProposition2,
          signatureProposition3,
          signatureProposition4,
          signatureProposition5
        ),
        3
      )
    )
    val signature1 = VerySecureSignatureRoutine.sign(sk1, signableBytes.value.toByteArray)
    val signature2 = VerySecureSignatureRoutine.sign(sk2, signableBytes.value.toByteArray)
    val signature3 = VerySecureSignatureRoutine.sign(sk3, signableBytes.value.toByteArray)
    val signatureProverProof1 = signatureProver.prove(Witness(ByteString.copyFrom(signature1)), signableBytes)
    val signatureProverProof2 = signatureProver.prove(Witness(ByteString.copyFrom(signature2)), signableBytes)
    val signatureProverProof3 = signatureProver.prove(Witness(ByteString.copyFrom(signature3)), signableBytes)
    val thresholdProverProof = thresholdProver.prove(
      Set(signatureProverProof1, signatureProverProof2, signatureProverProof3),
      signableBytes
    )
    val result = verifierInstance.evaluate(
      thresholdProposition,
      thresholdProverProof,
      dynamicContext(thresholdProposition, thresholdProverProof)
    )
    assertEquals(result.isLeft, true)
    assertEquals(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined,
      true
    )
  }

}
