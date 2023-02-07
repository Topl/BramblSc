package co.topl.brambl.validation

import cats.Id
import co.topl.brambl.MockHelpers
import cats.implicits._
import co.topl.brambl.models.{Datum, Event}
import co.topl.brambl.models.box.{Lock, Value}
import co.topl.brambl.models.transaction.{Attestation, Schedule}
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
      .exists(_.toList.contains(TransactionSyntaxError.DuplicateInput(inputFull.knownIdentifier)))
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
    val negativeValue: Value = Value().withToken(Value.Token(Int128(ByteString.copyFrom(BigInt(-1).toByteArray))))
    val testTx = txFull.copy(outputs = Seq(output.copy(value = negativeValue)))
    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator
      .validate(testTx)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.NonPositiveOutputValue(negativeValue)))
    assertEquals(result, true)
  }

  test("validate sufficient input funds") {
    val tokenValueIn: Value = Value().withToken(Value.Token(Int128(ByteString.copyFrom(BigInt(100).toByteArray))))
    val tokenValueOut: Value = Value().withToken(Value.Token(Int128(ByteString.copyFrom(BigInt(101).toByteArray))))
    val assetValueIn: Value =
      Value().withAsset(Value.Asset("label", Int128(ByteString.copyFrom(BigInt(100).toByteArray))))
    val assetValueOut: Value =
      Value().withAsset(Value.Asset("label", Int128(ByteString.copyFrom(BigInt(101).toByteArray))))

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
      testTx(assetValueIn, assetValueOut) // Asset Test
    ).forall(identity)
    assertEquals(result, true)
  }

  test("validate proof types: Lock.Predicate") {
    val challenges: Seq[Proposition] = List(
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
          Attestation().withPredicate(Attestation.Predicate(Lock.Predicate(challenges, 1), responses))
        )
      )
    )

    def testError(error: TransactionSyntaxError) = error match {
      case TransactionSyntaxError.InvalidProofType(challenge, response) =>
        // First challenge is mismatched so we expect the error
        if (challenge == challenges.head && response == responses.head) true else false
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
    val testTx = txFull.copy(datum =
      Datum
        .IoTransaction(
          Event
            .IoTransaction(
              Schedule(3, 50, 100),
              List(),
              List(),
              SmallData(invalidData).some
            )
        )
    )

    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator
      .validate(testTx)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.InvalidDataLength))
    assertEquals(result, true)
  }

  test(s"Valid data-length transaction with edge MaxDataLength") {
    import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
    import co.topl.brambl.common.ContainsImmutable.instances._
    val curSize = txFull
      .copy(datum =
        Datum
          .IoTransaction(
            Event
              .IoTransaction(
                Schedule(3, 50, 100),
                List(),
                List(),
                SmallData(ByteString.EMPTY).some
              )
          )
      )
      .immutable
      .value
      .size
    // create data with size -> tx + data = MaxDataLength
    val diff = TransactionSyntaxInterpreter.MaxDataLength - curSize + 1
    val result =
      if (diff > 0) {
        val invalidData = ByteString.copyFrom(Array.fill(diff)(1.toByte))
        val testTx = txFull.copy(datum =
          Datum
            .IoTransaction(
              Event
                .IoTransaction(
                  Schedule(3, 50, 100),
                  List(),
                  List(),
                  SmallData(invalidData).some
                )
            )
        )
        val validator = TransactionSyntaxInterpreter.make[Id]()
        validator
          .validate(testTx)
          .swap
          .exists(_.toList.contains(TransactionSyntaxError.InvalidDataLength))
      } else true
    assertEquals(result, true)
  }
}
