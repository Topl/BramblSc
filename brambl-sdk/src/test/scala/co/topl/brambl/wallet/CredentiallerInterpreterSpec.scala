package co.topl.brambl.wallet

import cats.Id
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.{Context, MockDataApi, MockHelpers}
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.models.KnownIdentifier
import co.topl.brambl.models.box.Value
import co.topl.brambl.validation.{TransactionAuthorizationError, TransactionSyntaxError}
import com.google.protobuf.ByteString
import quivr.models.Int128

// TODO: Replace/Update MockHelpers with the usage of ModelGenerators. TSDK-307
class CredentiallerInterpreterSpec extends munit.FunSuite with MockHelpers {

  test("prove: Single Input Transaction with Attestation.Predicate > Provable propositions have non-empty proofs") {
    val provenTx: IoTransaction = CredentiallerInterpreter.make[Id](MockDataApi).prove(txFull)
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    val sameLen = provenPredicate.lock.challenges.length == provenPredicate.responses.length
    val nonEmpty = provenPredicate.responses.forall(proof => !proof.value.isEmpty)
    assertEquals(sameLen && nonEmpty, true)
    assertEquals(provenTx.signable.value, txFull.signable.value)
  }

  test("prove: Single Input Transaction with Attestation.Predicate > Unprovable propositions have empty proofs") {
    // Secrets are not available for this KnownIdentifier
    val unknownKnownId =
      KnownIdentifier().withTransactionOutput32(dummyTxIdentifier.copy(network = 1, ledger = 1, index = 1))
    val testTx = txFull.copy(inputs = txFull.inputs.map(stxo => stxo.copy(knownIdentifier = unknownKnownId)))
    val provenTx: IoTransaction = CredentiallerInterpreter.make[Id](MockDataApi).prove(testTx)
    val provenPredicate = provenTx.inputs.head.attestation.getPredicate
    val sameLen = provenPredicate.lock.challenges.length == provenPredicate.responses.length
    val numEmpty = provenPredicate.responses.count(_.value.isEmpty) == 2 // Digest and Signature proofs are empty
    assertEquals(sameLen && numEmpty, true)
    assertEquals(provenTx.signable.value, testTx.signable.value)
  }

  test("validate: Single Input Transaction with Attestation.Predicate > Validation successful") {
    val credentialler = CredentiallerInterpreter.make[Id](MockDataApi)
    val provenTx: IoTransaction = credentialler.prove(txFull)
    val ctx = Context[Id](txFull, 50, _ => None) // Tick satisfies a proposition
    val errsNum = credentialler.validate(provenTx, ctx).length
    // Although not all but propositions pass, threshold is met so authorization is successful
    // Transaction syntax is also valid
    assertEquals(errsNum, 0)
  }

  test("validate: Single Input Transaction with Attestation.Predicate > Validation failed") {
    val negativeValue: Value = Value().withToken(Value.Token(Int128(ByteString.copyFrom(BigInt(-1).toByteArray))))
    val testTx = txFull.copy(outputs = Seq(output.copy(value = negativeValue)))
    val credentialler = CredentiallerInterpreter.make[Id](MockDataApi)
    val provenTx: IoTransaction = credentialler.prove(testTx)
    val ctx = Context[Id](txFull, 500, _ => None) // Tick does not satisfies proposition
    val errs = credentialler.validate(provenTx, ctx)
    // Threshold is not met so authorization failed
    // Transaction syntax is also invalid
    assertEquals(errs.length, 2)
    val result =
      errs.contains(TransactionSyntaxError.NonPositiveOutputValue(negativeValue)) &&
      errs.contains(TransactionAuthorizationError.AuthorizationFailed)
    assertEquals(result, true)
  }

  test("proveAndValidate: Single Input Transaction with Attestation.Predicate > Validation successful") {
    val credentialler = CredentiallerInterpreter.make[Id](MockDataApi)
    val ctx = Context[Id](txFull, 50, _ => None) // Tick satisfies a proposition
    val res = credentialler.proveAndValidate(txFull, ctx)

    assertEquals(res.isRight, true)
  }

  test("proveAndValidate: Single Input Transaction with Attestation.Predicate > Validation failed") {
    val negativeValue: Value = Value().withToken(Value.Token(Int128(ByteString.copyFrom(BigInt(-1).toByteArray))))
    val credentialler = CredentiallerInterpreter.make[Id](MockDataApi)
    val testTx = txFull.copy(outputs = Seq(output.copy(value = negativeValue)))
    val ctx = Context[Id](testTx, 500, _ => None) // Tick does not satisfies proposition
    val res = credentialler.proveAndValidate(testTx, ctx)
    assertEquals(res.isLeft, true)
    assertEquals(res.swap.getOrElse(List.empty).length, 2)
  }
}
