package co.topl.brambl.wallet

import cats.data.ValidatedNel
import cats.effect.IO
import cats.implicits._
import co.topl.brambl.builders.locks.LockTemplate
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.dataApi.WalletStateAlgebra
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{Datum, Event, Indices, LockAddress}
import co.topl.brambl.syntax.{cryptoToPbKeyPair, pbKeyPairToCryptoKeyPair}
import co.topl.brambl.validation.TransactionAuthorizationError.AuthorizationFailed
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.brambl.{Context, MockHelpers, MockWalletKeyApi, MockWalletStateApi}
import co.topl.crypto.generation.Bip32Indexes
import co.topl.crypto.signing.ExtendedEd25519
import co.topl.quivr.api.Proposer
import co.topl.quivr.runtime.QuivrRuntimeErrors.ValidationError.{EvaluationAuthorizationFailed, LockedPropositionIsUnsatisfiable}
import com.google.protobuf.ByteString
import munit.CatsEffectSuite
import quivr.models._

import scala.util.Random

class CredentiallerInterpreterSpec extends CatsEffectSuite with MockHelpers {
  val walletApi: WalletApi[F] = WalletApi.make[F](MockWalletKeyApi)

  test("prove: other fields on transaction are preserved") {
    val testTx = txFull
      .withGroupPolicies(Seq(Datum.GroupPolicy(mockGroupPolicy)))
      .withSeriesPolicies(Seq(Datum.SeriesPolicy(mockSeriesPolicy)))
      .withMintingStatements(Seq(AssetMintingStatement(dummyTxoAddress, dummyTxoAddress, quantity)))
    assertIO(
      for {
        provenTx <- CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
      } yield {
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val sameLen = provenPredicate.lock.challenges.length == provenPredicate.responses.length
        val nonEmpty = provenPredicate.responses.forall(proof => !proof.value.isEmpty)
        sameLen && nonEmpty && (provenTx.signable.value == testTx.signable.value)
      },
      true
    )
  }

  test("prove: Single Input Transaction with Attestation.Predicate > Provable propositions have non-empty proofs") {
    assertIO(
      for {
        provenTx <- CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair).prove(txFull)
      } yield {
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val sameLen = provenPredicate.lock.challenges.length == provenPredicate.responses.length
        val nonEmpty = provenPredicate.responses.forall(proof => !proof.value.isEmpty)
        sameLen && nonEmpty && (provenTx.signable.value == txFull.signable.value)
      },
      true
    )
  }

  test("prove: Single Input Transaction with Attestation.Predicate > Unprovable propositions have empty proofs") {
    assertIO(
      for {
        // Secrets are not available for the updated Signature and Digest propositions
        testSignatureProposition <- Proposer.signatureProposer[F].propose(("invalid-routine", MockChildKeyPair.vk))
        testDigestProposition    <- Proposer.digestProposer[F].propose(("invalid-routine", MockDigest))
        testTx <- {
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
          IO.pure(txFull.copy(inputs = txFull.inputs.map(stxo => stxo.copy(attestation = testAttestation))))
        }
        provenTx <- CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
      } yield {
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val sameLen = provenPredicate.lock.challenges.length == provenPredicate.responses.length
        val correctLen = provenPredicate.lock.challenges.length == 2
        val allEmpty = provenPredicate.responses.forall(_.value.isEmpty) // Digest and Signature proofs are empty
        sameLen && correctLen && allEmpty && (provenTx.signable.value == testTx.signable.value)
      },
      true
    )
  }

  test("proveAndValidate: Single Input Transaction with Digest Propositions (Blake2b256 and Sha256)") {
    assertIO(
      for {
        testTx <- {
          val testAttestation = Attestation().withPredicate(
            Attestation.Predicate(
              Lock.Predicate(
                List(
                  Challenge().withRevealed(MockDigestProposition), // Blake2b256
                  Challenge().withRevealed(MockSha256DigestProposition) // Sha256
                ),
                2 // Both are required
              ),
              List(Proof(), Proof())
            )
          )
          IO.pure(txFull.copy(inputs = txFull.inputs.map(stxo => stxo.copy(attestation = testAttestation))))
        }
        ctx = Context[F](testTx, 50, _ => None) // Tick and height are trivial
        // Secrets for the digests are available in the MockWalletStateApi
        validateRes <- CredentiallerInterpreter
          .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
          .proveAndValidate(testTx, ctx)
      } yield validateRes.isRight,
      // If successful, we know that we can prove and validate a transaction with Blake2b256 and Sha256 digest propositions
      true
    )
  }

  test("validate: Single Input Transaction with Attestation.Predicate > Validation successful") {
    val ctx = Context[F](txFull, 50, _ => None) // Tick satisfies a proposition
    val credentialler = CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
    assertIO(
      for {
        provenTx <- credentialler.prove(txFull)
        errs     <- credentialler.validate(provenTx, ctx)
        // Although not all but propositions pass, threshold is met so authorization is successful
        // Transaction syntax is also valid
      } yield errs.length.isEmpty,
      true
    )
  }

  test("validate: Single Input Transaction with Attestation.Predicate > Validation failed") {
    val negativeValue: Value =
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(-1).toByteArray))))
    val testTx = txFull.copy(outputs = Seq(output.copy(value = negativeValue)))
    val credentialler = CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
    val ctx = Context[F](testTx, 500, _ => None) // Tick does not satisfies proposition
    val provenTxWrapped = credentialler.prove(testTx)
    val errsWrapped = provenTxWrapped.flatMap(credentialler.validate(_, ctx))
    assertIO(
      errsWrapped.map(_.length == 2),
      true,
      "Threshold is not met so authorization failed and Transaction syntax is also invalid"
    )
    assertIO(
      errsWrapped.map(_.contains(TransactionSyntaxError.NonPositiveOutputValue(negativeValue))),
      true,
      "NonPositiveOutputValue Syntax Error is expected"
    )
    assertIO(
      errsWrapped.map(_.tail.head.isInstanceOf[AuthorizationFailed]),
      true,
      "AuthorizationFailed error is expected"
    )
    assertIO(
      errsWrapped.map(_.tail.head.asInstanceOf[AuthorizationFailed].errors.length == 3),
      true,
      "AuthorizationFailed error expects exactly 3 errors"
    )
    assertIO(
      errsWrapped.map(_.tail.head.asInstanceOf[AuthorizationFailed].errors.contains(LockedPropositionIsUnsatisfiable)),
      true,
      s"AuthorizationFailed error expects errors Locked error. Received: ${errsWrapped
          .map(_.tail.head.asInstanceOf[AuthorizationFailed].errors)}"
    )
    assertIO(
      for {
        provenAttestation <- provenTxWrapped.map(_.inputs.head.attestation.getPredicate)
        errs              <- errsWrapped
      } yield errs.tail.head
        .asInstanceOf[AuthorizationFailed]
        .errors
        // TODO: fix .getRevealed once implemented on the node
        .contains(
          EvaluationAuthorizationFailed(
            provenAttestation.lock.challenges(3).getRevealed,
            provenAttestation.responses(3)
          )
        ),
      true,
      s"AuthorizationFailed error expects errors Height error. Received: ${errsWrapped
          .map(_.tail.head.asInstanceOf[AuthorizationFailed].errors)}"
    )
    assertIO(
      for {
        provenAttestation <- provenTxWrapped.map(_.inputs.head.attestation.getPredicate)
        errs              <- errsWrapped
      } yield errs.tail.head
        .asInstanceOf[AuthorizationFailed]
        .errors
        // TODO: fix .getRevealed once implemented on the node
        .contains(
          EvaluationAuthorizationFailed(
            provenAttestation.lock.challenges(4).getRevealed,
            provenAttestation.responses(4)
          )
        ),
      true,
      s"AuthorizationFailed error expects errors Tick error. Received: ${errsWrapped
          .map(_.tail.head.asInstanceOf[AuthorizationFailed].errors)}"
    )
  }

  test("proveAndValidate: Single Input Transaction with Attestation.Predicate > Validation successful") {
    val credentialler = CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
    val ctx = Context[F](txFull, 50, _ => None) // Tick satisfies a proposition
    assertIO(
      credentialler.proveAndValidate(txFull, ctx).map(_.isRight),
      true
    )
  }

  test("proveAndValidate: Single Input Transaction with Attestation.Predicate > Validation failed") {
    val negativeValue: Value =
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(-1).toByteArray))))
    val credentialler = CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
    val testTx = txFull.copy(outputs = Seq(output.copy(value = negativeValue)))
    val ctx = Context[F](testTx, 500, _ => None) // Tick does not satisfies proposition
    assertIO(
      for {
        res <- credentialler.proveAndValidate(testTx, ctx)
      } yield res.isLeft && (res.swap.getOrElse(List.empty).length == 2),
      true
    )
  }

  test(
    "proveAndValidate: Credentialler initialized with a main key different than used to create Single Input Transaction with Attestation.Predicate > Validation Failed"
  ) {
    // Tick satisfies its proposition. Height does not.
    val ctx = Context[F](txFull, 50, _ => None)
    val differentKeyPair = WalletApi.make[F](MockWalletKeyApi).deriveChildKeys(MockMainKeyPair, Indices(0, 0, 1))
    val credentialler = differentKeyPair.map(CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, _))
    val res = credentialler.flatMap(_.proveAndValidate(txFull, ctx))
    // The DigitalSignature proof fails. Including Locked and Height failure, 3 errors are expected
    val errs = res.map(_.left.getOrElse(List.empty).head.asInstanceOf[AuthorizationFailed].errors)
    assertIO(
      res.map(_.isLeft),
      true,
      s"Result expecting to be left. Received ${res}"
    )
    assertIO(
      errs.map(_.length == 3),
      true,
      s"AuthorizationFailed errors expects exactly 3 errors. Received: ${errs.map(_.length)}"
    )
    assertIO(
      errs.map(_.exists {
        case EvaluationAuthorizationFailed(Proposition(Proposition.Value.DigitalSignature(_), _), _) => true
        case _                                                                                       => false
      }),
      true,
      s"AuthorizationFailed errors expects a DigitalSignature error. Received: ${errs}"
    )
  }

  test("prove: Transaction with Threshold Proposition > Threshold Proof is correctly generated") {
    assertIO(
      for {
        testProposition <- Proposer
          .thresholdProposer[F]
          .propose((Set(MockTickProposition, MockHeightProposition), 2))
          .map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        provenTx <- CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
      } yield {
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val validLength = provenPredicate.responses.length == 1
        val threshProof = provenPredicate.responses.head
        val validThreshold = (!threshProof.value.isEmpty) && (threshProof.value.isThreshold)
        val innerProofs = threshProof.value.threshold.get.responses
        val validProofs =
          (innerProofs.length == 2) && (innerProofs.head.value.isTickRange) && (innerProofs(1).value.isHeightRange)
        val validSignable = provenTx.signable.value == testTx.signable.value
        validLength && validThreshold && validProofs && validSignable
      },
      true
    )
  }

  test("prove: Transaction with And Proposition > And Proof is correctly generated") {
    assertIO(
      for {
        testProposition <- Proposer
          .andProposer[F]
          .propose((MockTickProposition, MockHeightProposition))
          .map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        provenTx <- CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
      } yield {
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val validLength = provenPredicate.responses.length == 1
        val andProof = provenPredicate.responses.head
        val validAnd =
          (!andProof.value.isEmpty) && (andProof.value.isAnd) && (andProof.value.and.get.left.value.isTickRange) && (andProof.value.and.get.right.value.isHeightRange)
        val validSignable = provenTx.signable.value == testTx.signable.value
        validLength && validAnd && validSignable
      },
      true
    )

  }

  test("prove: Transaction with Or Proposition > Or Proof is correctly generated") {
    assertIO(
      for {
        testProposition <- Proposer
          .orProposer[F]
          .propose((MockTickProposition, MockHeightProposition))
          .map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        provenTx <- CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
      } yield {
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val validLength = provenPredicate.responses.length == 1
        val orProof = provenPredicate.responses.head
        val validAnd =
          (!orProof.value.isEmpty) && (orProof.value.isOr) && (orProof.value.or.get.left.value.isTickRange) && (orProof.value.or.get.right.value.isHeightRange)
        val validSignable = provenTx.signable.value == testTx.signable.value
        validLength && validAnd && validSignable
      },
      true
    )
  }

  test("prove: Transaction with Not Proposition > Not Proof is correctly generated") {
    assertIO(
      for {
        testProposition <- Proposer.notProposer[F].propose(MockTickProposition).map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        provenTx <- CredentiallerInterpreter.make[F](walletApi, MockWalletStateApi, MockMainKeyPair).prove(testTx)
      } yield {
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val validLength = provenPredicate.responses.length == 1
        val notProof = provenPredicate.responses.head
        val validAnd =
          (!notProof.value.isEmpty) && (notProof.value.isNot) && (notProof.value.not.get.proof.value.isTickRange)
        val validSignable = provenTx.signable.value == testTx.signable.value
        validLength && validAnd && validSignable
      },
      true
    )
  }

  test("proveAndValidate: Transaction with Threshold Proposition > Unmet Threshold fails validation") {
    assertIO(
      for {
        testProposition <- Proposer
          .thresholdProposer[F]
          .propose(Set(MockTickProposition, MockHeightProposition), 2)
          .map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        ctx = Context[F](testTx, 50, _ => None) // Tick should pass, height should fail
        provenTx <- CredentiallerInterpreter
          .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
          .proveAndValidate(testTx, ctx)
      } yield {
        val validationErrs = provenTx.swap.getOrElse(List.empty)
        val validLength = validationErrs.length == 1

        val quivrErrs = validationErrs.head.asInstanceOf[AuthorizationFailed].errors
        val validQuivrErrs = (quivrErrs.length == 1) && (quivrErrs.head.isInstanceOf[EvaluationAuthorizationFailed])
        val err = quivrErrs.head.asInstanceOf[EvaluationAuthorizationFailed]
        val validThreshold =
          (err.proposition == testProposition.getRevealed) && (err.proof.value.isThreshold) && (err.proof.value.threshold.get.responses.head.value.isTickRange) && (err.proof.value.threshold.get
            .responses(1)
            .value
            .isHeightRange)
        validLength && validQuivrErrs && validThreshold
      },
      true
    )
  }

  test("proveAndValidate: Transaction with And Proposition > If one inner proof fails, the And proof fails") {
    assertIO(
      for {
        testProposition <- Proposer
          .andProposer[F]
          .propose((MockTickProposition, MockHeightProposition))
          .map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        ctx = Context[F](testTx, 50, _ => None) // Tick should pass, height should fail
        provenTx <- CredentiallerInterpreter
          .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
          .proveAndValidate(testTx, ctx)
      } yield {
        val validationErrs = provenTx.swap.getOrElse(List.empty)
        val validLength = validationErrs.length == 1
        val quivrErrs = validationErrs.head.asInstanceOf[AuthorizationFailed].errors
        val validQuivrErrs = (quivrErrs.length == 1) && (quivrErrs.head.isInstanceOf[EvaluationAuthorizationFailed])
        val err = quivrErrs.head.asInstanceOf[EvaluationAuthorizationFailed]
        // If an AND proposition fails, the error of the failed inner proof is returned. In this case it is the Height
        val validAnd = (err.proposition == MockHeightProposition) && (err.proof.value.isHeightRange)
        validLength && validQuivrErrs && validAnd
      },
      true
    )
  }

  test("proveAndValidate: Transaction with Or Proposition > If both inner proofs fail, the Or proof fails") {
    assertIO(
      for {
        testProposition <- Proposer
          .orProposer[F]
          .propose((MockTickProposition, MockHeightProposition))
          .map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        ctx = Context[F](testTx, 500, _ => None) // Tick and height should fail
        provenTx <- CredentiallerInterpreter
          .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
          .proveAndValidate(testTx, ctx)
      } yield {
        val validationErrs = provenTx.swap.getOrElse(List.empty)
        val validLength = validationErrs.length == 1
        val quivrErrs = validationErrs.head.asInstanceOf[AuthorizationFailed].errors
        val validQuivrErrs = (quivrErrs.length == 1) && (quivrErrs.head.isInstanceOf[EvaluationAuthorizationFailed])
        val err = quivrErrs.head.asInstanceOf[EvaluationAuthorizationFailed]
        val validOr =
          (err.proposition == testProposition.getRevealed) && (err.proof.value.isOr) && (err.proof.value.or.get.left.value.isTickRange) && (err.proof.value.or.get.right.value.isHeightRange)
        validLength && validQuivrErrs && validOr
      },
      true
    )
  }

  test("proveAndValidate: Transaction with Not Proposition > If inner proof succeeds, the Not proof fails") {
    assertIO(
      for {
        testProposition <- Proposer.notProposer[F].propose(MockTickProposition).map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        ctx = Context[F](testTx, 50, _ => None) // Tick should pass
        provenTx <- CredentiallerInterpreter
          .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
          .proveAndValidate(testTx, ctx)
      } yield {
        val validationErrs = provenTx.swap.getOrElse(List.empty)
        val validLength = validationErrs.length == 1
        val quivrErrs = validationErrs.head.asInstanceOf[AuthorizationFailed].errors
        val validQuivrErrs = (quivrErrs.length == 1) && (quivrErrs.head.isInstanceOf[EvaluationAuthorizationFailed])
        val err = quivrErrs.head.asInstanceOf[EvaluationAuthorizationFailed]
        val validOr =
          (err.proposition == testProposition.getRevealed) && (err.proof.value.isNot) && (err.proof.value.not.get.proof.value.isTickRange)
        validLength && validQuivrErrs && validOr
      },
      true
    )
  }

  test("proveAndValidate: Transaction with Threshold Proposition > Threshold met passes validation") {
    assertIO(
      for {
        testProposition <- Proposer
          .thresholdProposer[F]
          .propose(Set(MockTickProposition, MockHeightProposition), 2)
          .map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        // Both Tick and Height should pass
        ctx = Context[F](testTx, 50, Map("header" -> Datum().withHeader(Datum.Header(Event.Header(50)))).lift)
        res <- CredentiallerInterpreter
          .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
          .proveAndValidate(testTx, ctx)
      } yield {
        val provenTx: IoTransaction = res.toOption.get
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val validLength = provenPredicate.responses.length == 1
        val threshProof = provenPredicate.responses.head
        val validThreshold =
          (!threshProof.value.isEmpty) && (threshProof.value.isThreshold) && (threshProof.value.threshold.get.responses.head.value.isTickRange) && (threshProof.value.threshold.get
            .responses(1)
            .value
            .isHeightRange)
        val validSignable = provenTx.signable.value == testTx.signable.value
        validLength && validThreshold && validSignable
      },
      true
    )
  }

  test("proveAndValidate: Transaction with And Proposition > If both inner proofs pass, the And proof passes") {
    assertIO(
      for {
        testProposition <- Proposer
          .andProposer[F]
          .propose((MockTickProposition, MockHeightProposition))
          .map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        // Both Tick and Height should pass
        ctx = Context[F](testTx, 50, Map("header" -> Datum().withHeader(Datum.Header(Event.Header(50)))).lift)
        res <- CredentiallerInterpreter
          .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
          .proveAndValidate(testTx, ctx)
      } yield {
        val provenTx: IoTransaction = res.toOption.get
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val validLength = provenPredicate.responses.length == 1
        val andProof = provenPredicate.responses.head
        val validAnd =
          (!andProof.value.isEmpty) && (andProof.value.isAnd) && (andProof.value.and.get.left.value.isTickRange) && (andProof.value.and.get.right.value.isHeightRange)
        val validSignable = provenTx.signable.value == testTx.signable.value
        validLength && validAnd && validSignable
      },
      true
    )
  }

  test("proveAndValidate: Transaction with Or Proposition > If only one inner proof passes, the Or proof passes") {
    assertIO(
      for {
        testProposition <- Proposer
          .orProposer[F]
          .propose((MockTickProposition, MockHeightProposition))
          .map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        ctx = Context[F](testTx, 50, _ => None) // Tick should pass, height should fail
        res <- CredentiallerInterpreter
          .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
          .proveAndValidate(testTx, ctx)
      } yield {
        val provenTx: IoTransaction = res.toOption.get
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val validLength = provenPredicate.responses.length == 1
        val orProof = provenPredicate.responses.head
        val validOr =
          (!orProof.value.isEmpty) && (orProof.value.isOr) && (orProof.value.or.get.left.value.isTickRange) && (orProof.value.or.get.right.value.isHeightRange)
        val validSignable = provenTx.signable.value == testTx.signable.value
        validLength && validOr && validSignable
      },
      true
    )
  }

  test("proveAndValidate: Transaction with Not Proposition > If inner proof fails with error, the Not proof succeeds") {
    assertIO(
      for {
        // Locked should cause quivr runtime error
        testProposition <- Proposer.notProposer[F].propose(MockLockedProposition).map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        ctx = Context[F](testTx, 50, _ => None)
        res <- CredentiallerInterpreter
          .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
          .proveAndValidate(testTx, ctx)
      } yield {
        val provenTx: IoTransaction = res.toOption.get
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val validLength = provenPredicate.responses.length == 1
        val notProof = provenPredicate.responses.head
        val validNot =
          (!notProof.value.isEmpty) && (notProof.value.isNot) && (notProof.value.not.get.proof.value.isLocked)
        val validSignable = provenTx.signable.value == testTx.signable.value
        validLength && validNot && validSignable
      },
      true
    )
  }

  test(
    "proveAndValidate: Transaction with Not Proposition > If inner proof fails with `false`, the Not proof succeeds"
  ) {
    assertIO(
      for {
        testProposition <- Proposer.notProposer[F].propose(MockTickProposition).map(Challenge().withRevealed(_))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(testProposition), 1), List(Proof()))
              )
            )
          )
        )
        ctx = Context[F](testTx, 500, _ => None) // Tick should fail
        res <- CredentiallerInterpreter
          .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
          .proveAndValidate(testTx, ctx)
      } yield {
        val provenTx: IoTransaction = res.toOption.get
        val provenPredicate = provenTx.inputs.head.attestation.getPredicate
        val validLength = provenPredicate.responses.length == 1
        val notProof = provenPredicate.responses.head
        val validNot =
          (!notProof.value.isEmpty) && (notProof.value.isNot) && (notProof.value.not.get.proof.value.isTickRange)
        val validSignable = provenTx.signable.value == testTx.signable.value
        validLength && validNot && validSignable
      },
      true
    )
  }

  test(
    "prove: complex partially proven transaction"
  ) {
    val aliceSignatureProposition = MockSignatureProposition
    val aliceMainKey = MockMainKeyPair
    val edInstance = new ExtendedEd25519
    val bobMainKey: KeyPair = edInstance.deriveKeyPairFromSeed(Random.nextBytes(96))
    val bobIndices = Indices(8, 9, 10)
    val bobChildKey: KeyPair = edInstance.deriveKeyPairFromChildPath(
      pbKeyPairToCryptoKeyPair(bobMainKey).signingKey,
      List(
        Bip32Indexes.HardenedIndex(bobIndices.x),
        Bip32Indexes.SoftIndex(bobIndices.y),
        Bip32Indexes.SoftIndex(bobIndices.z)
      )
    )
    val bobSignatureProposition = Proposer.signatureProposer[F].propose(("ExtendedEd25519", bobChildKey.vk))
    // To Mock someone else's DataApi
    object NewWalletStateApi extends WalletStateAlgebra[F] {

      override def getInteractionList(fellowship: String, template: String): F[Option[List[(Indices, String)]]] = ???

      override def setCurrentIndices(fellowship: String, template: String, interaction: Int): F[Option[Indices]] = ???

      // The only relevant call is getIndices
      override def getIndicesBySignature(
        signatureProposition: Proposition.DigitalSignature
      ): F[Option[Indices]] =
        bobSignatureProposition.map(p =>
          Map(p.value.digitalSignature.get.sizedEvidence -> bobIndices).get(signatureProposition.sizedEvidence)
        )

      override def initWalletState(networkId:     Int, ledgerId:    Int, mainKey: KeyPair): F[Unit] = ???
      override def getPreimage(digestProposition: Proposition.Digest): F[Option[Preimage]] = ???
      override def addPreimage(preimage:          Preimage, digest: Proposition.Digest): IO[Unit] = ???
      override def getCurrentAddress: F[String] = ???
      override def updateWalletState(
        lockPredicate: String,
        lockAddress:   String,
        routine:       Option[String],
        vk:            Option[String],
        indices:       Indices
      ): F[Unit] = ???
      override def getCurrentIndicesForFunds(
        fellowship: String,
        contract:   String,
        someState:  Option[Int]
      ): F[Option[Indices]] = ???
      override def validateCurrentIndicesForFunds(
        fellowship: String,
        contract:   String,
        someState:  Option[Int]
      ): F[ValidatedNel[String, Indices]] = ???
      override def getNextIndicesForFunds(fellowship: String, contract: String): F[Option[Indices]] = ???
      override def getLockByIndex(indices:            Indices): F[Option[Lock.Predicate]] = ???
      override def getAddress(fellowship:   String, contract: String, someState: Option[Int]): F[Option[String]] = ???
      override def addEntityVks(fellowship: String, contract: String, entities:  List[String]): F[Unit] = ???
      override def getEntityVks(fellowship: String, contract: String): F[Option[List[String]]] = ???
      override def addNewLockTemplate(contract: String, lockTemplate: LockTemplate[F]): F[Unit] = ???
      override def getLockTemplate(contract:    String): F[Option[LockTemplate[F]]] = ???
      override def getLock(fellowship:          String, contract:     String, nextState: Int): F[Option[Lock]] = ???

      override def getLockByAddress(lockAddress: String): F[Option[Lock.Predicate]] = ???

      override def getIndicesByAddress(lockAddress: String): F[Option[Indices]] = ???

      override def getCurrentAddresses(includeGenesis: Boolean): F[Seq[(Indices, LockAddress)]] = ???
    }
    val aliceDataApi = MockWalletStateApi
    val bobDataApi = NewWalletStateApi
    // the following is used to mimic the initial proving of the transaction by alice.
    val credentialler1 = CredentiallerInterpreter.make[F](walletApi, aliceDataApi, aliceMainKey)
    // the following is used to mimic the second proving of the transaction by bob.
    val credentialler2 = CredentiallerInterpreter.make[F](walletApi, bobDataApi, bobMainKey)
    assertIO(
      for {
        andProp <- Proposer.andProposer[F].propose((MockTickProposition, aliceSignatureProposition))
        orProp  <- Proposer.orProposer[F].propose((MockDigestProposition, MockLockedProposition))
        notProp <- Proposer.notProposer[F].propose(MockHeightProposition)
        innerPropositions = List(andProp, orProp, notProp)
        bobProp <- bobSignatureProposition
        thresh1 <- Proposer.thresholdProposer[F].propose((innerPropositions.toSet, innerPropositions.length))
        thresh2 <- Proposer
          .thresholdProposer[F]
          .propose(((innerPropositions :+ bobProp).toSet, innerPropositions.length + 1))
        testTx = txFull.copy(inputs =
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(
                  Lock.Predicate(
                    List(thresh1, thresh2).map(Challenge().withRevealed),
                    2
                  ),
                  List.fill(2)(Proof())
                )
              )
            )
          )
        )
        ctx = Context[F](testTx, 50, _ => None) // Tick should pass, height should fail
        partiallyProven <- credentialler1.prove(testTx) // should create a partially proven tx
        // Should not be validated since not sufficiently proven
        res1             <- credentialler1.validate(partiallyProven, ctx)
        completelyProven <- credentialler2.prove(partiallyProven) // should create a completely proven tx
        // Should be validated since sufficiently proven
        res2 <- credentialler2.validate(completelyProven, ctx)
      } yield {
        val validRes1 = (res1.length == 1) && (partiallyProven.signable.value == testTx.signable.value)
        val validRes2 = (res2.isEmpty) && (completelyProven.signable.value == testTx.signable.value)
        validRes1 && validRes2
      },
      true
    )
  }
}
