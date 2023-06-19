package co.topl.brambl.wallet

import cats.Id
import cats.data.ValidatedNel
import cats.implicits._
import co.topl.brambl.builders.locks.LockTemplate
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.models.transaction.{IoTransaction, UnspentTransactionOutput}
import co.topl.brambl.{Context, MockWalletKeyApi, MockHelpers, MockWalletStateApi}
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.dataApi.WalletKeyApiAlgebra
import co.topl.brambl.models.{Datum, Event, Indices, LockAddress, TransactionOutputAddress}
import co.topl.brambl.models.box.{Attestation, Challenge, Lock, Value}
import co.topl.brambl.validation.TransactionAuthorizationError.AuthorizationFailed
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.crypto.generation.Bip32Indexes
import co.topl.crypto.signing.ExtendedEd25519
import co.topl.quivr.api.Proposer
import co.topl.quivr.runtime.QuivrRuntimeErrors.ValidationError.{
  EvaluationAuthorizationFailed,
  LockedPropositionIsUnsatisfiable
}
import com.google.protobuf.ByteString
import quivr.models.{Int128, KeyPair, Preimage, Proof, Proposition, VerificationKey}
import co.topl.brambl.wallet.WalletApi.{cryptoToPbKeyPair, pbKeyPairToCryotoKeyPair}
import co.topl.crypto.encryption.VaultStore

import scala.util.Random

class CredentiallerInterpreterSpec extends munit.FunSuite with MockHelpers {
  val walletApi: WalletApi[Id] = WalletApi.make[Id](MockWalletKeyApi)

  test("prove: Single Input Transaction with Attestation.Predicate > Provable propositions have non-empty proofs") {
    val provenTx: IoTransaction =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).prove(txFull)
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
        List(Proof(), Proof())
      )
    )
    val testTx = txFull.copy(inputs = txFull.inputs.map(stxo => stxo.copy(attestation = testAttestation)))
    val provenTx: IoTransaction =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    val sameLen = provenPredicate.lock.challenges.length == provenPredicate.responses.length
    val correctLen = provenPredicate.lock.challenges.length == 2
    val allEmpty = provenPredicate.responses.forall(_.value.isEmpty) // Digest and Signature proofs are empty
    assert(sameLen && correctLen && allEmpty)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test("validate: Single Input Transaction with Attestation.Predicate > Validation successful") {
    val credentialler = CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair)
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
    val credentialler = CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair)
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
    val credentialler = CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair)
    val ctx = Context[Id](txFull, 50, _ => None) // Tick satisfies a proposition
    val res = credentialler.proveAndValidate(txFull, ctx)

    assertEquals(res.isRight, true)
  }

  test("proveAndValidate: Single Input Transaction with Attestation.Predicate > Validation failed") {
    val negativeValue: Value =
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(-1).toByteArray))))
    val credentialler = CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair)
    val testTx = txFull.copy(outputs = Seq(output.copy(value = negativeValue)))
    val ctx = Context[Id](testTx, 500, _ => None) // Tick does not satisfies proposition
    val res = credentialler.proveAndValidate(testTx, ctx)
    assertEquals(res.isLeft, true)
    assertEquals(res.swap.getOrElse(List.empty).length, 2)
  }

  test(
    "proveAndValidate: Credentialler initialized with a main key different than used to create Single Input Transaction with Attestation.Predicate > Validation Failed"
  ) {
    val differentKeyPair = WalletApi.make[Id](MockWalletKeyApi).deriveChildKeys(MockMainKeyPair, Indices(0, 0, 1))
    val credentialler = CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, differentKeyPair)
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
    val testProposition =
      Challenge().withRevealed(
        Proposer.thresholdProposer[Id].propose((Set(MockTickProposition, MockHeightProposition), 2))
      )
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val provenTx: IoTransaction =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    assert(provenPredicate.responses.length == 1)
    val threshProof = provenPredicate.responses.head
    assert(!threshProof.value.isEmpty)
    assert(threshProof.value.isThreshold)
    val innerProofs = threshProof.value.threshold.get.responses
    assert(innerProofs.length == 2)
    assert(innerProofs.head.value.isTickRange)
    assert(innerProofs(1).value.isHeightRange)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test("prove: Transaction with And Proposition > And Proof is correctly generated") {
    val testProposition =
      Challenge().withRevealed(Proposer.andProposer[Id].propose((MockTickProposition, MockHeightProposition)))
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val provenTx: IoTransaction =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    assert(provenPredicate.responses.length == 1)
    val andProof = provenPredicate.responses.head
    assert(!andProof.value.isEmpty)
    assert(andProof.value.isAnd)
    assert(andProof.value.and.get.left.value.isTickRange)
    assert(andProof.value.and.get.right.value.isHeightRange)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test("prove: Transaction with Or Proposition > Or Proof is correctly generated") {
    val testProposition =
      Challenge().withRevealed(Proposer.orProposer[Id].propose((MockTickProposition, MockHeightProposition)))
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val provenTx: IoTransaction =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    assert(provenPredicate.responses.length == 1)
    val orProof = provenPredicate.responses.head
    assert(!orProof.value.isEmpty)
    assert(orProof.value.isOr)
    assert(orProof.value.or.get.left.value.isTickRange)
    assert(orProof.value.or.get.right.value.isHeightRange)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test("prove: Transaction with Not Proposition > Not Proof is correctly generated") {
    val testProposition = Challenge().withRevealed(Proposer.notProposer[Id].propose(MockTickProposition))
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val provenTx: IoTransaction =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    assert(provenPredicate.responses.length == 1)
    val notProof = provenPredicate.responses.head
    assert(!notProof.value.isEmpty)
    assert(notProof.value.isNot)
    assert(notProof.value.not.get.proof.value.isTickRange)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test("proveAndValidate: Transaction with Threshold Proposition > Unmet Threshold fails validation") {
    val testProposition =
      Challenge().withRevealed(
        Proposer.thresholdProposer[Id].propose(Set(MockTickProposition, MockHeightProposition), 2)
      )
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val ctx = Context[Id](testTx, 50, _ => None) // Tick should pass, height should fail
    val res =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).proveAndValidate(testTx, ctx)
    assert(res.isLeft)
    val validationErrs = res.swap.getOrElse(List.empty)
    assert(validationErrs.length == 1)
    val quivrErrs = validationErrs.head.asInstanceOf[AuthorizationFailed].errors
    assert(quivrErrs.length == 1)
    assert(quivrErrs.head.isInstanceOf[EvaluationAuthorizationFailed])
    val err = quivrErrs.head.asInstanceOf[EvaluationAuthorizationFailed]
    assert(err.proposition == testProposition.getRevealed)
    assert(err.proof.value.isThreshold)
    assert(err.proof.value.threshold.get.responses.head.value.isTickRange)
    assert(err.proof.value.threshold.get.responses(1).value.isHeightRange)
  }

  test("proveAndValidate: Transaction with And Proposition > If one inner proof fails, the And proof fails") {
    val testProposition =
      Challenge().withRevealed(Proposer.andProposer[Id].propose((MockTickProposition, MockHeightProposition)))
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val ctx = Context[Id](testTx, 50, _ => None) // Tick should pass, height should fail
    val res =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).proveAndValidate(testTx, ctx)
    assert(res.isLeft)
    val validationErrs = res.swap.getOrElse(List.empty)
    assert(validationErrs.length == 1)
    val quivrErrs = validationErrs.head.asInstanceOf[AuthorizationFailed].errors
    assert(quivrErrs.length == 1)
    assert(quivrErrs.head.isInstanceOf[EvaluationAuthorizationFailed])
    // If an AND proposition fails, the error of the failed inner proof is returned. In this case it is the Height
    assert(quivrErrs.head.asInstanceOf[EvaluationAuthorizationFailed].proposition == MockHeightProposition)
    assert(quivrErrs.head.asInstanceOf[EvaluationAuthorizationFailed].proof.value.isHeightRange)
  }

  test("proveAndValidate: Transaction with Or Proposition > If both inner proofs fail, the Or proof fails") {
    val testProposition =
      Challenge().withRevealed(Proposer.orProposer[Id].propose((MockTickProposition, MockHeightProposition)))
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val ctx = Context[Id](testTx, 500, _ => None) // Tick and height should fail
    val res =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).proveAndValidate(testTx, ctx)
    assert(res.isLeft)
    val validationErrs = res.swap.getOrElse(List.empty)
    assert(validationErrs.length == 1)
    val quivrErrs = validationErrs.head.asInstanceOf[AuthorizationFailed].errors
    assert(quivrErrs.length == 1)
    assert(quivrErrs.head.isInstanceOf[EvaluationAuthorizationFailed])
    val err = quivrErrs.head.asInstanceOf[EvaluationAuthorizationFailed]
    assert(err.proposition == testProposition.getRevealed)
    assert(err.proof.value.isOr)
    assert(err.proof.value.or.get.left.value.isTickRange)
    assert(err.proof.value.or.get.right.value.isHeightRange)
  }

  test("proveAndValidate: Transaction with Not Proposition > If inner proof succeeds, the Not proof fails") {
    val testProposition = Challenge().withRevealed(Proposer.notProposer[Id].propose(MockTickProposition))
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val ctx = Context[Id](testTx, 50, _ => None) // Tick should pass
    val res =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).proveAndValidate(testTx, ctx)
    assert(res.isLeft)
    val validationErrs = res.swap.getOrElse(List.empty)
    assert(validationErrs.length == 1)
    val quivrErrs = validationErrs.head.asInstanceOf[AuthorizationFailed].errors
    assert(quivrErrs.length == 1)
    assert(quivrErrs.head.isInstanceOf[EvaluationAuthorizationFailed])
    val err = quivrErrs.head.asInstanceOf[EvaluationAuthorizationFailed]
    assert(err.proposition == testProposition.getRevealed)
    assert(err.proof.value.isNot)
    assert(err.proof.value.not.get.proof.value.isTickRange)
  }

  test("proveAndValidate: Transaction with Threshold Proposition > Threshold met passes validation") {
    val testProposition =
      Challenge().withRevealed(
        Proposer.thresholdProposer[Id].propose(Set(MockTickProposition, MockHeightProposition), 2)
      )
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    // Both Tick and Height should pass
    val ctx = Context[Id](testTx, 50, Map("header" -> Datum().withHeader(Datum.Header(Event.Header(50)))).lift)
    val res =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).proveAndValidate(testTx, ctx)
    assert(res.isRight)
    val provenTx: IoTransaction = res.toOption.get
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    assert(provenPredicate.responses.length == 1)
    val threshProof = provenPredicate.responses.head
    assert(!threshProof.value.isEmpty)
    assert(threshProof.value.isThreshold)
    assert(threshProof.value.threshold.get.responses.head.value.isTickRange)
    assert(threshProof.value.threshold.get.responses(1).value.isHeightRange)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test("proveAndValidate: Transaction with And Proposition > If both inner proofs pass, the And proof passes") {
    val testProposition =
      Challenge().withRevealed(Proposer.andProposer[Id].propose((MockTickProposition, MockHeightProposition)))
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    // Both Tick and Height should pass
    val ctx = Context[Id](testTx, 50, Map("header" -> Datum().withHeader(Datum.Header(Event.Header(50)))).lift)
    val res =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).proveAndValidate(testTx, ctx)
    assert(res.isRight)
    val provenTx: IoTransaction = res.toOption.get
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    assert(provenPredicate.responses.length == 1)
    val andProof = provenPredicate.responses.head
    assert(!andProof.value.isEmpty)
    assert(andProof.value.isAnd)
    assert(andProof.value.and.get.left.value.isTickRange)
    assert(andProof.value.and.get.right.value.isHeightRange)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test("proveAndValidate: Transaction with Or Proposition > If only one inner proof passes, the Or proof passes") {
    val testProposition =
      Challenge().withRevealed(Proposer.orProposer[Id].propose((MockTickProposition, MockHeightProposition)))
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val ctx = Context[Id](testTx, 50, _ => None) // Tick should pass, height should fail
    val res =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).proveAndValidate(testTx, ctx)
    assert(res.isRight)
    val provenTx: IoTransaction = res.toOption.get
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    assert(provenPredicate.responses.length == 1)
    val orProof = provenPredicate.responses.head
    assert(!orProof.value.isEmpty)
    assert(orProof.value.isOr)
    assert(orProof.value.or.get.left.value.isTickRange)
    assert(orProof.value.or.get.right.value.isHeightRange)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test("proveAndValidate: Transaction with Not Proposition > If inner proof fails with error, the Not proof succeeds") {
    // Locked should cause quivr runtime error
    val testProposition = Challenge().withRevealed(Proposer.notProposer[Id].propose(MockLockedProposition))
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val ctx = Context[Id](testTx, 50, _ => None)
    val res =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).proveAndValidate(testTx, ctx)
    assert(res.isRight)
    val provenTx: IoTransaction = res.toOption.get
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    assert(provenPredicate.responses.length == 1)
    val notProof = provenPredicate.responses.head
    assert(!notProof.value.isEmpty)
    assert(notProof.value.isNot)
    assert(notProof.value.not.get.proof.value.isLocked)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test(
    "proveAndValidate: Transaction with Not Proposition > If inner proof fails with `false`, the Not proof succeeds"
  ) {
    val testProposition = Challenge().withRevealed(Proposer.notProposer[Id].propose(MockTickProposition))
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof())))
        )
      )
    )
    val ctx = Context[Id](testTx, 500, _ => None) // Tick should fail
    val res =
      CredentiallerInterpreter.make[Id](walletApi, MockWalletStateApi, MockMainKeyPair).proveAndValidate(testTx, ctx)
    assert(res.isRight)
    val provenTx: IoTransaction = res.toOption.get
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    assert(provenPredicate.responses.length == 1)
    val notProof = provenPredicate.responses.head
    assert(!notProof.value.isEmpty)
    assert(notProof.value.isNot)
    assert(notProof.value.not.get.proof.value.isTickRange)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test(
    "prove: complex partially proven transaction"
  ) {
    val aliceSignatureProposition = MockSignatureProposition
    val aliceMainKey = MockMainKeyPair
    val bobMainKey: KeyPair = (new ExtendedEd25519).deriveKeyPairFromSeed(Random.nextBytes(96))
    val bobIndices = Indices(8, 9, 10)
    val bobChildKey: KeyPair = (new ExtendedEd25519).deriveKeyPairFromChildPath(
      pbKeyPairToCryotoKeyPair(bobMainKey).signingKey,
      List(
        Bip32Indexes.HardenedIndex(bobIndices.x),
        Bip32Indexes.SoftIndex(bobIndices.y),
        Bip32Indexes.SoftIndex(bobIndices.z)
      )
    )
    val bobSignatureProposition = Proposer.signatureProposer[Id].propose(("ExtendedEd25519", bobChildKey.vk))
    // To Mock someone else's DataApi
    object NewWalletStateApi extends WalletStateAlgebra[Id] {
      // The only relevant call is getIndices
      override def getIndicesBySignature(
        signatureProposition: Proposition.DigitalSignature
      ): Id[Option[Indices]] =
        Map(
          bobSignatureProposition.value.digitalSignature.get.sizedEvidence -> bobIndices
        ).get(signatureProposition.sizedEvidence)

      override def initWalletState(vk:            VerificationKey): Id[Unit] = ???
      override def getPreimage(digestProposition: Proposition.Digest): Id[Option[Preimage]] = ???
      override def getCurrentAddress: Id[String] = ???
      override def updateWalletState(
        lockPredicate: String,
        lockAddress:   String,
        routine:       Option[String],
        vk:            Option[String],
        indices:       Indices
      ): Id[Unit] = ???
      override def getCurrentIndicesForFunds(
        party:     String,
        contract:  String,
        someState: Option[Int]
      ): Id[Option[Indices]] = ???
      override def validateCurrentIndicesForFunds(
        party:     String,
        contract:  String,
        someState: Option[Int]
      ): Id[ValidatedNel[String, Indices]] = ???
      override def getNextIndicesForFunds(party: String, contract: String): Id[Option[Indices]] = ???
      override def getLockByIndex(indices:       Indices): Id[Option[Lock.Predicate]] = ???
      override def getAddress(party:   String, contract: String, someState: Option[Int]): Id[Option[String]] = ???
      override def addEntityVks(party: String, contract: String, entities:  List[String]): Id[Unit] = ???
      override def getEntityVks(party: String, contract: String): Id[Option[List[String]]] = ???
      override def addNewLockTemplate(contract: String, lockTemplate: LockTemplate[Id]): Id[Unit] = ???
      override def getLockTemplate(contract:    String): Id[Option[LockTemplate[Id]]] = ???
      override def getLock(party:               String, contract:     String, nextState: Int): Id[Option[Lock]] = ???
    }
    val aliceDataApi = MockWalletStateApi
    val bobDataApi = NewWalletStateApi
    val innerPropositions = List(
      Proposer.andProposer[Id].propose((MockTickProposition, aliceSignatureProposition)),
      Proposer.orProposer[Id].propose((MockDigestProposition, MockLockedProposition)),
      Proposer.notProposer[Id].propose(MockHeightProposition)
    )
    val testTx = txFull.copy(inputs =
      List(
        inputFull.copy(attestation =
          Attestation().withPredicate(
            Attestation.Predicate(
              Lock.Predicate(
                List(
                  Proposer.thresholdProposer[Id].propose((innerPropositions.toSet, innerPropositions.length)),
                  Proposer
                    .thresholdProposer[Id]
                    .propose(((innerPropositions :+ bobSignatureProposition).toSet, innerPropositions.length + 1))
                ).map(Challenge().withRevealed),
                2
              ),
              List.fill(2)(Proof())
            )
          )
        )
      )
    )
    val ctx = Context[Id](testTx, 50, _ => None) // Tick should pass, height should fail
    // the following is used to mimic the initial proving of the transaction by alice.
    val credentialler1 = CredentiallerInterpreter.make[Id](walletApi, aliceDataApi, aliceMainKey)
    val partiallyProven = credentialler1.prove(testTx) // should create a partially proven tx
    // Should not be validated since not sufficiently proven
    val res1 = credentialler1.validate(partiallyProven, ctx)
    assert(res1.length == 1)
    assertEquals(partiallyProven.signable.value, testTx.signable.value)
    // the following is used to mimic the second proving of the transaction by bob.
    val credentialler2 = CredentiallerInterpreter.make[Id](walletApi, bobDataApi, bobMainKey)
    val completelyProven = credentialler2.prove(partiallyProven) // should create a completely proven tx
    // Should be validated since sufficiently proven
    val res2 = credentialler2.validate(completelyProven, ctx)
    assert(res2.isEmpty)
    assertEquals(completelyProven.signable.value, testTx.signable.value)
  }
}
