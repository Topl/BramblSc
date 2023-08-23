package co.topl.brambl.validation

import cats.Id
import cats.implicits._
import co.topl.brambl.MockHelpers
import co.topl.brambl.models.box.{Attestation, Challenge, Lock, Value}
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{Datum, Event}
import co.topl.quivr.api.{Proposer, Prover}
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof, Proposition, SmallData}
import scala.language.implicitConversions

class TransactionSyntaxInterpreterSpec extends munit.FunSuite with MockHelpers {

  test("validate non-empty inputs") {
    val testTx = txFull.copy(inputs = List())
    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator
      .validate(testTx)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.EmptyInputs))
    assertEquals(result, true)
  }

  test("validate distinct inputs") {
    val testTx = txFull.copy(inputs = List(inputFull, inputFull))
    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator
      .validate(testTx)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.DuplicateInput(inputFull.address)))
    assertEquals(result, true)
  }

  test("validate maximum outputs count") {
    val testTx = txFull.copy(outputs = Vector.fill(Short.MaxValue)(output))
    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator
      .validate(testTx)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.ExcessiveOutputsCount))
    assertEquals(result, true)
  }

  test("validate positive timestamp") {
    val testTx = txFull.copy(datum =
      txDatum
        .copy(
          event = txDatum.event.copy(schedule = Schedule(3, 50, -1))
        )
    )
    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator
      .validate(testTx)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.InvalidTimestamp(-1)))
    assertEquals(result, true)
  }

  test("validate schedule") {
    val invalidSchedules = List(Schedule(5, 4, 100), Schedule(-5, -1, 100), Schedule(-1, 0, 100), Schedule(-1, 1, 100))
    val result = invalidSchedules
      .map { schedule =>
        val testTx = txFull.copy(datum =
          txDatum
            .copy(
              event = txDatum.event.copy(schedule = schedule)
            )
        )
        val validator = TransactionSyntaxInterpreter.make[Id]()
        validator
          .validate(testTx)
          .swap
          .exists(_.toList.contains(TransactionSyntaxError.InvalidSchedule(schedule)))
      }
      .forall(identity)
    assertEquals(result, true)
  }

  test("validate positive output quantities") {
    val negativeValue: Value =
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(-1).toByteArray))))
    val testTx = txFull.copy(outputs = Seq(output.copy(value = negativeValue)))
    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator
      .validate(testTx)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.NonPositiveOutputValue(negativeValue)))
    assertEquals(result, true)
  }

  test("validate sufficient input funds") {
    val tokenValueIn: Value =
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(100).toByteArray))))
    val tokenValueOut: Value =
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(101).toByteArray))))
    val assetValueIn: Value =
      Value.defaultInstance.withAsset(
        Value.Asset("label", Int128(ByteString.copyFrom(BigInt(100).toByteArray)), SmallData())
      )
    val assetValueOut: Value =
      Value.defaultInstance.withAsset(
        Value.Asset("label", Int128(ByteString.copyFrom(BigInt(101).toByteArray)), SmallData())
      )

    val groupValueIn: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          quantity = Int128(ByteString.copyFrom(BigInt(100).toByteArray)),
          groupPolicy = Event.GroupPolicy(label = "groupLabel", registrationUtxo = dummyTxoAddress)
        )
      )

    val groupValueOut: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          quantity = Int128(ByteString.copyFrom(BigInt(101).toByteArray)),
          groupPolicy = Event.GroupPolicy(label = "groupLabel", registrationUtxo = dummyTxoAddress)
        )
      )

    val seriesValueIn: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          quantity = Int128(ByteString.copyFrom(BigInt(100).toByteArray)),
          seriesPolicy = Event.SeriesPolicy(label = "groupLabel", registrationUtxo = dummyTxoAddress)
        )
      )

    val seriesValueOut: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          quantity = Int128(ByteString.copyFrom(BigInt(101).toByteArray)),
          seriesPolicy = Event.SeriesPolicy(label = "groupLabel", registrationUtxo = dummyTxoAddress)
        )
      )

    def testTx(inputValue: Value, outputValue: Value) = TransactionSyntaxInterpreter
      .make[Id]()
      .validate(
        txFull.copy(
          inputs = txFull.inputs.map(_.copy(value = inputValue)),
          outputs = Seq(output.copy(value = outputValue))
        )
      )
      .swap
      .exists(
        _.toList
          .contains(TransactionSyntaxError.InsufficientInputFunds(List(inputValue.value), List(outputValue.value)))
      )

    val result = List(
      testTx(tokenValueIn, tokenValueOut), // Token Test
      testTx(assetValueIn, assetValueOut), // Asset Test
      testTx(groupValueIn, groupValueOut), // Group Test
      testTx(seriesValueIn, seriesValueOut) // Series Test
    ).forall(identity)
    assertEquals(result, true)
  }

  test("validate proof types: Lock.Predicate") {
    val propositions: Seq[Proposition] = List(
      Proposer.LockedProposer[Id].propose(None),
      Proposer.heightProposer[Id].propose(("header", 0, 100)),
      Proposer.tickProposer[Id].propose((0, 100)),
      Proposer.tickProposer[Id].propose((0, 100))
    )
    val responses: Seq[Proof] = List(
      Prover.heightProver[Id].prove((), fakeMsgBind), // Mismatched
      Prover.heightProver[Id].prove((), fakeMsgBind), // Matched
      Proof() // Empty proof
      // Missing a Proof
    )
    val testTx = txFull.copy(inputs =
      txFull.inputs.map(
        _.copy(attestation =
          Attestation().withPredicate(
            Attestation.Predicate(Lock.Predicate(propositions.map(Challenge().withRevealed), 1), responses)
          )
        )
      )
    )

    def testError(error: TransactionSyntaxError) = error match {
      case TransactionSyntaxError.InvalidProofType(challenge, response) =>
        // First challenge is mismatched so we expect the error
        if (challenge == propositions.head && response == responses.head) true else false
      case _ => false // We don't expect any other errors
    }

    val result = TransactionSyntaxInterpreter
      .make[Id]()
      .validate(testTx)
      .swap
      .forall(_.toList.map(testError).forall(identity))
    assertEquals(result, true)
  }

  test("Invalid data-length transaction > MaxDataLength ") {
    val invalidData = ByteString.copyFrom(Array.fill(TransactionSyntaxInterpreter.MaxDataLength + 1)(1.toByte))
    val testTx = txFull.copy(outputs = List.fill(5000)(output))

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator
      .validate(testTx)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.InvalidDataLength))
    assertEquals(result, true)
  }

  test("validate that Group outputs that try to reference not LVLs input") {

    val valueTopl: Value =
      Value.defaultInstance.withTopl(Value.TOPL(Int128(ByteString.copyFrom(BigInt(1).toByteArray))))
    val input = SpentTransactionOutput(dummyTxoAddress, attFull, valueTopl)
    val txFull = IoTransaction.defaultInstance.withInputs(List(input)).withDatum(txDatum)
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = dummyTxoAddress)

    val groupA: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)),
          groupPolicy = groupPolicy
        )
      )
    val output1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, groupA)
    val testTx = txFull.copy(outputs = List(output1), groupPolicy = Seq(Datum.GroupPolicy(groupPolicy)))
    val validator = TransactionSyntaxInterpreter.make[Id]()

    val result = validator
      .validate(testTx)
      .swap
      .exists(
        _.toList.contains(
          TransactionSyntaxError.InsufficientInputFunds(
            testTx.inputs.map(_.value.value).toList,
            testTx.outputs.map(_.value.value).toList
          )
        )
      )
    assertEquals(result, true)
  }

  test("validate that Series outputs that try to reference not LVLs input") {

    val valueTopl: Value =
      Value.defaultInstance.withTopl(Value.TOPL(Int128(ByteString.copyFrom(BigInt(1).toByteArray))))
    val input = SpentTransactionOutput(dummyTxoAddress, attFull, valueTopl)
    val txFull = IoTransaction.defaultInstance.withInputs(List(input)).withDatum(txDatum)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelA", registrationUtxo = dummyTxoAddress)

    val series: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)),
          seriesPolicy = seriesPolicy
        )
      )
    val output1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, series)
    val testTx = txFull.copy(outputs = List(output1), seriesPolicy = Seq(Datum.SeriesPolicy(seriesPolicy)))
    val validator = TransactionSyntaxInterpreter.make[Id]()

    val result = validator
      .validate(testTx)
      .swap
      .exists(
        _.toList.contains(
          TransactionSyntaxError.InsufficientInputFunds(
            testTx.inputs.map(_.value.value).toList,
            testTx.outputs.map(_.value.value).toList
          )
        )
      )
    assertEquals(result, true)
  }

  test("validate that Group Contructor tokens includes the policy in the Iotx Datum") {

    val valueTopl: Value =
      Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(1).toByteArray))))
    val input = SpentTransactionOutput(dummyTxoAddress, attFull, valueTopl)
    val txFull = IoTransaction.defaultInstance.withInputs(List(input)).withDatum(txDatum)
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = dummyTxoAddress)

    val group: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)),
          groupPolicy = groupPolicy
        )
      )
    val output1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, group)
    val testTx = txFull.copy(outputs = List(output1), groupPolicy = Seq.empty) // No policy
    val validator = TransactionSyntaxInterpreter.make[Id]()

    val result = validator
      .validate(testTx)
      .swap
      .exists(
        _.toList.contains(
          TransactionSyntaxError.InsufficientInputFunds(
            testTx.inputs.map(_.value.value).toList,
            testTx.outputs.map(_.value.value).toList
          )
        )
      )
    assertEquals(result, true)
  }

  test("validate that Series Contructor tokens includes the policy in the Iotx Datum") {

    val valueTopl = Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(1).toByteArray))))
    val input = SpentTransactionOutput(dummyTxoAddress, attFull, valueTopl)
    val txFull = IoTransaction.defaultInstance.withInputs(List(input)).withDatum(txDatum)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelA", registrationUtxo = dummyTxoAddress)

    val series: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)),
          seriesPolicy = seriesPolicy
        )
      )
    val output1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, series)
    val testTx = txFull.copy(outputs = List(output1), seriesPolicy = Seq.empty) // No policy
    val validator = TransactionSyntaxInterpreter.make[Id]()

    val result = validator
      .validate(testTx)
      .swap
      .exists(
        _.toList.contains(
          TransactionSyntaxError.InsufficientInputFunds(
            testTx.inputs.map(_.value.value).toList,
            testTx.outputs.map(_.value.value).toList
          )
        )
      )
    assertEquals(result, true)
  }

  test("validate there aren't two Group outputs that try to reference the same LVL input") {

    val addressInput = txFull.inputs.head.address
    val groupPolicyA = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = addressInput)
    val groupPolicyB = Event.GroupPolicy(label = "groupLabelB", registrationUtxo = addressInput)

    val groupA: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)),
          groupPolicy = groupPolicyA
        )
      )
    val groupB: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)),
          groupPolicy = groupPolicyB
        )
      )
    val output1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, groupA)
    val output2: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, groupB)
    val testTx = txFull.copy(
      outputs = List(output1, output2),
      groupPolicy = Seq(Datum.GroupPolicy(groupPolicyA), Datum.GroupPolicy(groupPolicyB))
    )
    val validator = TransactionSyntaxInterpreter.make[Id]()

    val result = validator
      .validate(testTx)
      .swap
      .exists(
        _.toList.contains(
          TransactionSyntaxError.InsufficientInputFunds(
            testTx.inputs.map(_.value.value).toList,
            testTx.outputs.map(_.value.value).toList
          )
        )
      )
    assertEquals(result, true)
  }

  test("validate there aren't two Series outputs that try to reference the same LVL input") {

    val addressInput = txFull.inputs.head.address
    val seriesPolicyA = Event.SeriesPolicy(label = "seriesLabelA", registrationUtxo = addressInput)
    val seriesPolicyB = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = addressInput)

    val seriesA: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)),
          seriesPolicy = seriesPolicyA
        )
      )
    val seriesB: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)),
          seriesPolicy = seriesPolicyB
        )
      )
    val output1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, seriesA)
    val output2: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, seriesB)
    val testTx = txFull.copy(
      outputs = List(output1, output2),
      seriesPolicy = Seq(Datum.SeriesPolicy(seriesPolicyA), Datum.SeriesPolicy(seriesPolicyB))
    )
    val validator = TransactionSyntaxInterpreter.make[Id]()

    val result = validator
      .validate(testTx)
      .swap
      .exists(
        _.toList.contains(
          TransactionSyntaxError.InsufficientInputFunds(
            testTx.inputs.map(_.value.value).toList,
            testTx.outputs.map(_.value.value).toList
          )
        )
      )
    assertEquals(result, true)
  }

  test("validate there aren't a Group and a Series outputs that try to reference the same LVL input") {

    val addressInput = txFull.inputs.head.address
    val groupPolicy = Event.GroupPolicy(label = "groupLabelA", registrationUtxo = addressInput)
    val seriesPolicy = Event.SeriesPolicy(label = "seriesLabelB", registrationUtxo = addressInput)

    val group: Value =
      Value.defaultInstance.withGroup(
        Value.Group(
          quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)),
          groupPolicy = groupPolicy
        )
      )
    val series: Value =
      Value.defaultInstance.withSeries(
        Value.Series(
          quantity = Int128(ByteString.copyFrom(BigInt(1).toByteArray)),
          seriesPolicy = seriesPolicy
        )
      )
    val output1: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, group)
    val output2: UnspentTransactionOutput = UnspentTransactionOutput(trivialLockAddress, series)
    val testTx = txFull.copy(
      outputs = List(output1, output2),
      groupPolicy = Seq(Datum.GroupPolicy(groupPolicy)),
      seriesPolicy = Seq(Datum.SeriesPolicy(seriesPolicy))
    )
    val validator = TransactionSyntaxInterpreter.make[Id]()

    val result = validator
      .validate(testTx)
      .swap
      .exists(
        _.toList.contains(
          TransactionSyntaxError.InsufficientInputFunds(
            testTx.inputs.map(_.value.value).toList,
            testTx.outputs.map(_.value.value).toList
          )
        )
      )
    assertEquals(result, true)
  }

}
