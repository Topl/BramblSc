package co.topl.brambl.wallet

import cats.Id
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.{Context, MockDataApi, MockHelpers}
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.models.Indices
import co.topl.brambl.models.box.{Attestation, Challenge, Lock, Value}
import co.topl.brambl.validation.TransactionAuthorizationError.AuthorizationFailed
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.quivr.api.Proposer
import co.topl.quivr.runtime.QuivrRuntimeErrors.ValidationError.{
  EvaluationAuthorizationFailed,
  LockedPropositionIsUnsatisfiable
}
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proposition}

class CredentiallerInterpreterSpec extends munit.FunSuite with MockHelpers {

  test("prove: Single Input Transaction with Attestation.Predicate > Provable propositions have non-empty proofs") {
    val provenTx: IoTransaction = CredentiallerInterpreter.make[Id](MockDataApi, MockMainKeyPair).prove(txFull)
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    val sameLen = provenPredicate.lock.challenges.length == provenPredicate.responses.length
    val nonEmpty = provenPredicate.responses.forall(proof => !proof.value.isEmpty)
    assert(sameLen && nonEmpty)
    assertEquals(provenTx.signable.value, txFull.signable.value)
  }

  test("prove: Single Input Transaction with Attestation.Predicate > Unprovable propositions have empty proofs") {
    // Secrets are not available for the updated Signature and Digest propositions
    val testSignatureProposition = Proposer.signatureProposer[Id].propose(("invalid-routine", MockChildKeyPair.vk))
    val testDigestProposition = Proposer.digestProposer[Id].propose(("invalid-routine", MockDigest))
    val testAttestation = Attestation().withPredicate(
      Attestation.Predicate(
        Lock.Predicate(
          List(
            Challenge().withRevealed(testSignatureProposition),
            Challenge().withRevealed(testDigestProposition)
          ),
          2
        ),
        List()
      )
    )
    val testTx = txFull.copy(inputs = txFull.inputs.map(stxo => stxo.copy(attestation = testAttestation)))
    val provenTx: IoTransaction = CredentiallerInterpreter.make[Id](MockDataApi, MockMainKeyPair).prove(testTx)
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    val sameLen = provenPredicate.lock.challenges.length == provenPredicate.responses.length
    val correctLen = provenPredicate.lock.challenges.length == 2
    val allEmpty = provenPredicate.responses.forall(_.value.isEmpty) // Digest and Signature proofs are empty
    assert(sameLen && correctLen && allEmpty)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test("validate: Single Input Transaction with Attestation.Predicate > Validation successful") {
    val credentialler = CredentiallerInterpreter.make[Id](MockDataApi, MockMainKeyPair)
    val provenTx: IoTransaction = credentialler.prove(txFull)
    val ctx = Context[Id](txFull, 50, _ => None) // Tick satisfies a proposition
    val errsNum = credentialler.validate(provenTx, ctx).length
    // Although not all but propositions pass, threshold is met so authorization is successful
    // Transaction syntax is also valid
    assertEquals(errsNum, 0)
  }

  test("validate: Single Input Transaction with Attestation.Predicate > Validation failed") {
    val negativeValue: Value =
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(-1).toByteArray))))
    val testTx = txFull.copy(outputs = Seq(output.copy(value = negativeValue)))
    val credentialler = CredentiallerInterpreter.make[Id](MockDataApi, MockMainKeyPair)
    val provenTx: IoTransaction = credentialler.prove(testTx)
    val ctx = Context[Id](testTx, 500, _ => None) // Tick does not satisfies proposition
    val errs = credentialler.validate(provenTx, ctx)
    // Threshold is not met so authorization failed
    // Transaction syntax is also invalid
    assertEquals(errs.length, 2)
    assert(
      errs.contains(TransactionSyntaxError.NonPositiveOutputValue(negativeValue)),
      "NonPositiveOutputValue Syntax Error is expected"
    )
    val provenAttestation = provenTx.inputs.head.attestation.getPredicate
    assert(errs.tail.head.isInstanceOf[AuthorizationFailed], "AuthorizationFailed error is expected")
    assert(
      errs.tail.head.asInstanceOf[AuthorizationFailed].errors.length == 3,
      "AuthorizationFailed error expects exactly 3 errors"
    )
    assert(
      errs.tail.head.asInstanceOf[AuthorizationFailed].errors.contains(LockedPropositionIsUnsatisfiable),
      s"AuthorizationFailed error expects errors Locked error. Received: ${errs.tail.head.asInstanceOf[AuthorizationFailed].errors}"
    )
    assert(
      errs.tail.head
        .asInstanceOf[AuthorizationFailed]
        .errors
        // TODO: fix .getRevealed
        .contains(
          EvaluationAuthorizationFailed(
            provenAttestation.lock.challenges(3).getRevealed,
            provenAttestation.responses(3)
          )
        ),
      s"AuthorizationFailed error expects Height error. Received: ${errs.tail.head.asInstanceOf[AuthorizationFailed].errors}"
    )
    assert(
      errs.tail.head
        .asInstanceOf[AuthorizationFailed]
        .errors
        // TODO: fix .getRevealed
        .contains(
          EvaluationAuthorizationFailed(
            provenAttestation.lock.challenges(4).getRevealed,
            provenAttestation.responses(4)
          )
        ),
      s"AuthorizationFailed error expects Tick error. Received: ${errs.tail.head.asInstanceOf[AuthorizationFailed].errors}"
    )
  }

  test("proveAndValidate: Single Input Transaction with Attestation.Predicate > Validation successful") {
    val credentialler = CredentiallerInterpreter.make[Id](MockDataApi, MockMainKeyPair)
    val ctx = Context[Id](txFull, 50, _ => None) // Tick satisfies a proposition
    val res = credentialler.proveAndValidate(txFull, ctx)

    assertEquals(res.isRight, true)
  }

  test("proveAndValidate: Single Input Transaction with Attestation.Predicate > Validation failed") {
    val negativeValue: Value =
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(-1).toByteArray))))
    val credentialler = CredentiallerInterpreter.make[Id](MockDataApi, MockMainKeyPair)
    val testTx = txFull.copy(outputs = Seq(output.copy(value = negativeValue)))
    val ctx = Context[Id](testTx, 500, _ => None) // Tick does not satisfies proposition
    val res = credentialler.proveAndValidate(testTx, ctx)
    assertEquals(res.isLeft, true)
    assertEquals(res.swap.getOrElse(List.empty).length, 2)
  }

  test(
    "proveAndValidate: Credentialler initialized with a main key different than used to create Single Input Transaction with Attestation.Predicate > Validation Failed"
  ) {
    val differentKeyPair = WalletApi.make[Id](MockDataApi).deriveChildKeys(MockMainKeyPair, Indices(0, 0, 1))
    val credentialler = CredentiallerInterpreter.make[Id](MockDataApi, differentKeyPair)
    // Tick satisfies its proposition. Height does not.
    val ctx = Context[Id](txFull, 50, _ => None)
    val res = credentialler.proveAndValidate(txFull, ctx)

    assert(res.isLeft, s"Result expecting to be left. Received ${res}")
    // The DigitalSignature proof fails. Including Locked and Height failure, 3 errors are expected
    val errs = res.left.getOrElse(List.empty).head.asInstanceOf[AuthorizationFailed].errors
    assert(
      errs.length == 3,
      s"AuthorizationFailed errors expects exactly 3 errors. Received: ${errs.length}"
    )
    assert(
      errs.exists {
        case EvaluationAuthorizationFailed(Proposition(Proposition.Value.DigitalSignature(_), _), _) => true
        case _                                                                                       => false
      },
      s"AuthorizationFailed errors expects a DigitalSignature error. Received: ${errs}"
    )
  }

  test("prove: Transaction with Threshold Proposition > Threshold Proof is correctly generated") {
  }

  test("prove: Transaction with And Proposition > And Proof is correctly generated") {
  }

  test("prove: Transaction with Or Proposition > Or Proof is correctly generated") {
  }

  test("prove: Transaction with Not Proposition > Not Proof is correctly generated") {
  }

  test("proveAndValidate: Transaction with Threshold Proposition > Unmet Threshold fails validation") {
  }

  test("proveAndValidate: Transaction with And Proposition > If one inner proof fails, the And proof fails") {
  }

  test("proveAndValidate: Transaction with Or Proposition > If both inner proofs fail, the Or proof fails") {
  }

  test("proveAndValidate: Transaction with Not Proposition > If inner proof succeeds, the Not proof fails") {
  }
  test("proveAndValidate: Transaction with Threshold Proposition > Threshold met passes validation") {
  }

  test("proveAndValidate: Transaction with And Proposition > If both inner proofs pass, the And proof passes") {
  }

  test("proveAndValidate: Transaction with Or Proposition > If only one inner proof passes, the Or proof passes") {
  }

  test("proveAndValidate: Transaction with Not Proposition > If inner proof fails with error, the Not proof succeeds") {
  }
  test("proveAndValidate: Transaction with Not Proposition > If inner proof fails with `false`, the Not proof succeeds") {
  }
}
