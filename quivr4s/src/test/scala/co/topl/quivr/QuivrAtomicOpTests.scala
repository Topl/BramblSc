package co.topl.quivr

import cats.{Id, Monad}
import co.topl.brambl.models.Datum
import co.topl.crypto.hash.Blake2b256
import co.topl.quivr.runtime.QuivrRuntimeErrors
import com.google.protobuf.ByteString
import quivr.models.VerificationKey._
import quivr.models._

/**
 * Set of tests for the Quivr Atomic Operations.
 */
class QuivrAtomicOpTests extends munit.FunSuite with MockHelpers {

  import co.topl.quivr.api.Proposer._
  import co.topl.quivr.api.Prover._
  import co.topl.quivr.api.Verifier.instances._

  implicit val applicativeId: Monad[Id] = cats.catsInstancesForId

  test("A locked proposition must return an LockedPropositionIsUnsatisfiable when evaluated") {
    val lockedProposition = LockedProposer.propose(None)
    val lockedProverProof = lockedProver.prove((), signableBytes)
    val result = verifierInstance[Id, Datum].evaluate(
      lockedProposition,
      lockedProverProof,
      dynamicContext(lockedProposition, lockedProverProof)
    )
    assertEquals(result.isLeft, true)
    assertEquals(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.LockedPropositionIsUnsatisfiable =>
        true
      }.isDefined,
      true
    )
  }

  test("A tick proposition must evaluate to true when tick is in range") {
    val tickProposition = tickProposer.propose(900, 1000)
    val tickProverProof = tickProver.prove((), signableBytes)
    val result =
      verifierInstance[Id, Datum].evaluate(
        tickProposition,
        tickProverProof,
        dynamicContext(tickProposition, tickProverProof)
      )
    assertEquals(result.isRight, true)
  }

  test("A tick position must evaluate to false when the tick is not in range") {
    val tickProposition = tickProposer.propose(1, 10)
    val tickProverProof = tickProver.prove((), signableBytes)
    val result =
      verifierInstance[Id, Datum].evaluate(
        tickProposition,
        tickProverProof,
        dynamicContext(tickProposition, tickProverProof)
      )
    assertEquals(result.isLeft, true)
    assertEquals(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined,
      true
    )
  }

  test("A height proposition must evaluate to true when height is in range") {
    val heightProposition = heightProposer.propose("height", 900, 1000)
    val heightProverProof = heightProver.prove((), signableBytes)
    val result = verifierInstance[Id, Datum].evaluate(
      heightProposition,
      heightProverProof,
      dynamicContext(heightProposition, heightProverProof)
    )
    assertEquals(result.isRight, true)
  }

  test("A height proposition must evaluate to false when height is not in range") {
    val heightProposition = heightProposer.propose("height", 1, 10)
    val heightProverProof = heightProver.prove((), signableBytes)
    val result = verifierInstance[Id, Datum].evaluate(
      heightProposition,
      heightProverProof,
      dynamicContext(heightProposition, heightProverProof)
    )
    assertEquals(result.isLeft, true)
    assertEquals(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined,
      true
    )
  }

  test("A signature proposition must evaluate to true when the signature proof is correct") {
    val (sk, vk) = VerySecureSignatureRoutine.generateKeyPair()
    val signatureProposition = signatureProposer.propose(
      (
        "VerySecure",
        VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk))))
      )
    )
    val signature = VerySecureSignatureRoutine.sign(sk, signableBytes.value.toByteArray)
    val signatureProverProof = signatureProver.prove(Witness(ByteString.copyFrom(signature)), signableBytes)
    val result = verifierInstance[Id, Datum].evaluate(
      signatureProposition,
      signatureProverProof,
      dynamicContext(signatureProposition, signatureProverProof)
    )
    assertEquals(result.isRight, true)
  }

  test("A signature proposition must evaluate to false when the signature proof is not correct") {
    val (_, vk) = VerySecureSignatureRoutine.generateKeyPair()
    val (sk, _) = VerySecureSignatureRoutine.generateKeyPair()
    val signatureProposition = signatureProposer.propose(
      "VerySecure",
      VerificationKey(Vk.Ed25519(Ed25519Vk(ByteString.copyFrom(vk))))
    )
    val signature = VerySecureSignatureRoutine.sign(sk, signableBytes.value.toByteArray)
    val signatureProverProof = signatureProver.prove(Witness(ByteString.copyFrom(signature)), signableBytes)
    val result = verifierInstance[Id, Datum].evaluate(
      signatureProposition,
      signatureProverProof,
      dynamicContext(signatureProposition, signatureProverProof)
    )
    assertEquals(result.isLeft, true)
    assertEquals(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined,
      true
    )
  }

  test("A digest proposition must evaluate to true when the digest is correct") {
    val mySalt = ByteString.copyFromUtf8("I am a digest")
    val myPreimage = Preimage(ByteString.copyFromUtf8("I am a preimage"), mySalt)
    val myDigest = Digest(
      ByteString.copyFrom(
        (new Blake2b256).hash(myPreimage.input.toByteArray ++ myPreimage.salt.toByteArray)
      )
    )
    val digestProposition = digestProposer.propose(("blake2b256", myDigest))
    val digestProverProof = digestProver.prove(myPreimage, signableBytes)
    val result = verifierInstance[Id, Datum].evaluate(
      digestProposition,
      digestProverProof,
      dynamicContext(digestProposition, digestProverProof)
    )
    assertEquals(result.isRight, true)
  }

  test("A digest proposition must evaluate to false when the digest is incorrect") {
    val mySalt = ByteString.copyFromUtf8("I am a digest")
    val myPreimage = Preimage(ByteString.copyFromUtf8("I am a preimage"), mySalt)
    val myDigest = Digest(
      ByteString.copyFrom(
        (new Blake2b256).hash(myPreimage.input.toByteArray ++ myPreimage.salt.toByteArray)
      )
    )
    val wrongPreImage = Preimage(ByteString.copyFromUtf8("I am a wrong preimage"), mySalt)
    val digestProposition = digestProposer.propose(("blake2b256", myDigest))
    val digestProverProof = digestProver.prove(wrongPreImage, signableBytes)
    val result = verifierInstance[Id, Datum].evaluate(
      digestProposition,
      digestProverProof,
      dynamicContext(digestProposition, digestProverProof)
    )
    assertEquals(result.isLeft, true)
    assertEquals(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined,
      true
    )
  }

  test("Proposition and Proof with mismatched types fails validation") {
    val proposition = heightProposer.propose("height", 900, 1000)
    val proof = tickProver.prove((), signableBytes)
    val result = verifierInstance[Id, Datum].evaluate(
      proposition,
      proof,
      dynamicContext(proposition, proof)
    )
    assert(result.isLeft)
    assert(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined
    )
  }

  test("Empty Proof fails validation") {
    val proposition = heightProposer.propose("height", 900, 1000)
    val proof = Proof()
    val result = verifierInstance[Id, Datum].evaluate(
      proposition,
      proof,
      dynamicContext(proposition, proof)
    )
    assert(result.isLeft)
    assert(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined
    )
  }

  test("Empty Proposition fails validation") {
    val proposition = Proposition()
    val proof = tickProver.prove((), signableBytes)
    val result = verifierInstance[Id, Datum].evaluate(
      proposition,
      proof,
      dynamicContext(proposition, proof)
    )
    assert(result.isLeft)
    assert(
      result.left.toOption.collect { case QuivrRuntimeErrors.ValidationError.EvaluationAuthorizationFailed(_, _) =>
        true
      }.isDefined
    )
  }
}
