package co.topl.brambl.validation

import cats.Id
import cats.data.Chain
import co.topl.brambl.MockHelpers
import cats.implicits._
import co.topl.brambl.models.{Datum, Event}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{Attestation, IoTransaction, Schedule}
import co.topl.quivr.api.Prover
import com.google.protobuf.ByteString
import quivr.models.{Int128, Proof, SmallData}

import scala.language.implicitConversions

class TransactionSyntaxInterpreterSpec extends munit.FunSuite with MockHelpers {
  test("validate non-empty inputs") {
    val testTx  = txFull.copy(inputs = List())
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
      .exists(_.toList.contains(TransactionSyntaxError.DuplicateInput(inputFull.knownIdentifier.get)))
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
    val testTx = txFull.copy(datum = txDatum.copy(
      event = txDatum.event.get.copy(schedule = Schedule(3, 50, -1).some).some
    ).some)
    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator
      .validate(testTx)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.InvalidTimestamp(-1)))
    assertEquals(result, true)
  }

  test("validate schedule") {
    val invalidSchedules = List(Schedule(5, 4, 100), Schedule(-5, -1, 100), Schedule(-1, 0, 100), Schedule(-1, 1, 100))
    val result = invalidSchedules.map(schedule => {
      val testTx = txFull.copy(datum = txDatum.copy(
        event = txDatum.event.get.copy(schedule = schedule.some).some
      ).some)
      val validator = TransactionSyntaxInterpreter.make[Id]()
      validator
        .validate(testTx)
        .swap
        .exists(_.toList.contains(TransactionSyntaxError.InvalidSchedule(schedule)))
    }).forall(identity)
    assertEquals(result, true)
  }

  test("validate positive output quantities") {
    val negativeValue: Value = Value().withToken(Value.Token(Int128(ByteString.copyFrom(BigInt(-1).toByteArray)).some))
    val testTx = txFull.copy(outputs = Seq(output.copy(value = negativeValue.some)))
    val validator = TransactionSyntaxInterpreter.make[Id]()
    val result = validator
      .validate(testTx)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.NonPositiveOutputValue(negativeValue)))
    assertEquals(result, true)
  }

  test("validate sufficient input funds") {
    val tokenValueIn: Value = Value().withToken(Value.Token(Int128(ByteString.copyFrom(BigInt(100).toByteArray)).some))
    val tokenValueOut: Value = Value().withToken(Value.Token(Int128(ByteString.copyFrom(BigInt(101).toByteArray)).some))
    val assetValueIn: Value = Value().withAsset(Value.Asset("label", Int128(ByteString.copyFrom(BigInt(100).toByteArray)).some))
    val assetValueOut: Value = Value().withAsset(Value.Asset("label", Int128(ByteString.copyFrom(BigInt(101).toByteArray)).some))

    def testTx(transaction: IoTransaction) = TransactionSyntaxInterpreter.make[Id]()
        .validate(transaction)
        .swap
        .exists(_.toList.contains(TransactionSyntaxError.InsufficientInputFunds(_, _)))

    val result = List(
      // Token Test
      testTx(
        txFull.copy(inputs = txFull.inputs.map(_.copy(value = tokenValueIn.some)), outputs = Seq(output.copy(value = tokenValueOut.some)))
      ),
      // Asset Test
      testTx (
        txFull.copy(inputs = txFull.inputs.map(_.copy(value = assetValueIn.some)), outputs = Seq(output.copy(value = assetValueOut.some)))
      )
    ).forall(identity)
    assertEquals(result, true)
  }

  test("validate proof types") {
    val txUnproven = txEmpty
    val inResponsesMismatched: Seq[Proof] = List(
      Prover.heightProver[Id].prove((), fakeMsgBind),
      Prover.lockedProver[Id].prove((), fakeMsgBind),
      Prover.heightProver[Id].prove((), fakeMsgBind)
    )
    val txProven = txFull.copy(inputs = txFull.inputs.map(_.copy(attestation =
      Attestation().withPredicate(Attestation.Predicate(inLock.some, inResponsesMismatched)).some
    )))

    def testTx(transaction: IoTransaction) = TransactionSyntaxInterpreter.make[Id]()
      .validate(transaction)
      .swap
      .exists(_.toList.contains(TransactionSyntaxError.InvalidProofType(_, _)))

    val result = List(txUnproven, txProven).map(testTx).forall(identity)
    assertEquals(result, true)
  }

  test("Invalid data-length transaction > MaxDataLength ") {
    val invalidData = ByteString.copyFrom(Array.fill(TransactionSyntaxInterpreter.MaxDataLength + 1)(1.toByte))
    val testTx = txFull.copy(datum = Datum.IoTransaction(
      Event
        .IoTransaction(
          Schedule(3, 50, 100).some,
          List(),
          List(),
          SmallData(invalidData).some
        )
        .some
    ).some)

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
    val curSize = txFull.copy(datum = Datum.IoTransaction(
      Event
        .IoTransaction(
          Schedule(3, 50, 100).some,
          List(),
          List(),
          SmallData(ByteString.EMPTY).some
        )
        .some
    ).some).immutable.value.size
    // create data with size -> tx + data = MaxDataLength
    val diff = TransactionSyntaxInterpreter.MaxDataLength - curSize + 1
    val result =
    if(diff > 0){
      val invalidData = ByteString.copyFrom(Array.fill(diff)(1.toByte))
      val testTx = txFull.copy(datum = Datum.IoTransaction(
        Event
          .IoTransaction(
            Schedule(3, 50, 100).some,
            List(),
            List(),
            SmallData(invalidData).some
          )
          .some
      ).some)
      val validator = TransactionSyntaxInterpreter.make[Id]()
      validator
        .validate(testTx)
        .swap
        .exists(_.toList.contains(TransactionSyntaxError.InvalidDataLength))
    } else true
    assertEquals(result, true)
  }
}
