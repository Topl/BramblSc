package co.topl.brambl.display

import cats.effect.unsafe.implicits.global
import co.topl.brambl.Context
import co.topl.brambl.MockHelpers
import co.topl.brambl.MockWalletKeyApi
import co.topl.brambl.MockWalletStateApi
import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction.Schedule
import co.topl.brambl.syntax.bigIntAsInt128
import co.topl.brambl.syntax.longAsInt128
import co.topl.brambl.wallet.CredentiallerInterpreter
import co.topl.brambl.wallet.WalletApi
import co.topl.quivr.api.Proposer
import com.google.protobuf.ByteString
import quivr.models.Proof
import quivr.models.SmallData

class DisplaySpec extends munit.FunSuite with MockHelpers {

  test("Display Complex Transaction") {
    val testTx = txFull
      .withDatum(
        Datum.IoTransaction(
          Event
            .IoTransaction(
              Schedule(0, Long.MaxValue, System.currentTimeMillis),
              SmallData(ByteString.copyFrom("metadata".getBytes))
            )
        )
      )
      .withGroupPolicies(Seq(Datum.GroupPolicy(mockGroupPolicy)))
      .withSeriesPolicies(Seq(Datum.SeriesPolicy(mockSeriesPolicy)))
      .withMintingStatements(
        Seq(
          AssetMintingStatement(mockGroupPolicy.registrationUtxo, mockSeriesPolicy.registrationUtxo, 1)
        )
      )
      .withInputs(
        Seq(
          lvlValue,
          groupValue,
          seriesValue,
          assetGroupSeries
        ).map(value => inputFull.withValue(value))
      )
      .withOutputs(
        Seq(
          lvlValue,
          groupValue,
          seriesValue,
          assetGroupSeries
        ).map(value => output.withValue(value))
      )
    assertNoDiff(
      testTx.display.trim(),
      s"""
TransactionId              : 3G1TsJUpmurYxMnzQ8NaBrnK3DymVGzRKLUwLysinjb6

Group Policies
==============
Label                      : Mock Group Policy
Registration-Utxo          : 4pX2G4weCKBHDT9axEm3HChq6jURV7ZYRPgeb7KWkEzm#0
Fixed-Series               : NO FIXED SERIES

Series Policies
===============
Label                      : Mock Series Policy
Registration-Utxo          : 4pX2G4weCKBHDT9axEm3HChq6jURV7ZYRPgeb7KWkEzm#0
Fungibility                : group-and-series
Quantity-Descriptor        : liquid
Token-Supply               : UNLIMITED
Permanent-Metadata-Scheme  : \nNo permanent metadata
Ephemeral-Metadata-Scheme  : \nNo ephemeral metadata

Asset Minting Statements
========================
Group-Token-Utxo           : 4pX2G4weCKBHDT9axEm3HChq6jURV7ZYRPgeb7KWkEzm#0
Series-Token-Utxo          : 4pX2G4weCKBHDT9axEm3HChq6jURV7ZYRPgeb7KWkEzm#0
Quantity                   : 1
Permanent-Metadata         : \nNo permanent metadata

Asset Merging Statements
========================


Inputs
======
TxoAddress                 : 4pX2G4weCKBHDT9axEm3HChq6jURV7ZYRPgeb7KWkEzm#0
Attestation                : Not implemented
Type                       : LVL
Value                      : 1
-----------
TxoAddress                 : 4pX2G4weCKBHDT9axEm3HChq6jURV7ZYRPgeb7KWkEzm#0
Attestation                : Not implemented
Type                       : Group Constructor
Id                         : cabf98baf365915d2282eca423bfae4a6425bad6064b8d97f2c39ba6e9fceafb
Fixed-Series               : NO FIXED SERIES
Value                      : 1
-----------
TxoAddress                 : 4pX2G4weCKBHDT9axEm3HChq6jURV7ZYRPgeb7KWkEzm#0
Attestation                : Not implemented
Type                       : Series Constructor
Id                         : 094c5a3acf338bcca90c91c9adcae5f4b59dec385740e80660111a3d6b10a8ce
Fungibility                : group-and-series
Token-Supply               : UNLIMITED
Quant-Descr.               : liquid
Value                      : 1
-----------
TxoAddress                 : 4pX2G4weCKBHDT9axEm3HChq6jURV7ZYRPgeb7KWkEzm#0
Attestation                : Not implemented
Type                       : Asset
GroupId                    : cabf98baf365915d2282eca423bfae4a6425bad6064b8d97f2c39ba6e9fceafb
SeriesId                   : 094c5a3acf338bcca90c91c9adcae5f4b59dec385740e80660111a3d6b10a8ce
GroupAlloy                 : N/A
SeriesAlloy                : N/A
Commitment                 : No commitment
Ephemeral-Metadata         : \nNo ephemeral metadata
Value                      : 1

Outputs
=======
LockAddress                : 1111111145ALDDRQ2EubxAYgTNdCKvTaP6GZXEWzi2vz6JmTAvryHY6ok
Type                       : LVL
Value                      : 1
-----------
LockAddress                : 1111111145ALDDRQ2EubxAYgTNdCKvTaP6GZXEWzi2vz6JmTAvryHY6ok
Type                       : Group Constructor
Id                         : cabf98baf365915d2282eca423bfae4a6425bad6064b8d97f2c39ba6e9fceafb
Fixed-Series               : NO FIXED SERIES
Value                      : 1
-----------
LockAddress                : 1111111145ALDDRQ2EubxAYgTNdCKvTaP6GZXEWzi2vz6JmTAvryHY6ok
Type                       : Series Constructor
Id                         : 094c5a3acf338bcca90c91c9adcae5f4b59dec385740e80660111a3d6b10a8ce
Fungibility                : group-and-series
Token-Supply               : UNLIMITED
Quant-Descr.               : liquid
Value                      : 1
-----------
LockAddress                : 1111111145ALDDRQ2EubxAYgTNdCKvTaP6GZXEWzi2vz6JmTAvryHY6ok
Type                       : Asset
GroupId                    : cabf98baf365915d2282eca423bfae4a6425bad6064b8d97f2c39ba6e9fceafb
SeriesId                   : 094c5a3acf338bcca90c91c9adcae5f4b59dec385740e80660111a3d6b10a8ce
GroupAlloy                 : N/A
SeriesAlloy                : N/A
Commitment                 : No commitment
Ephemeral-Metadata         : \nNo ephemeral metadata
Value                      : 1

Datum
=====
Value                      : KJHK1EAZuVA
""".trim()
    )
  }

  test("Display Validation Errors") {
    val walletApi: WalletApi[F] = WalletApi.make[F](MockWalletKeyApi)
    val vErrs = for {
      andProp <- Proposer.andProposer[F].propose((MockHeightProposition, MockSignatureProposition))
      orProp  <- Proposer.orProposer[F].propose((MockDigestProposition, MockLockedProposition))
      notProp <- Proposer.notProposer[F].propose(MockTickProposition)
      innerPropositions = List(andProp, notProp, orProp)
      thresh <- Proposer.thresholdProposer[F].propose((innerPropositions.toSet, innerPropositions.length))
      testTx = txFull
        .withInputs(
          List(
            inputFull.copy(attestation =
              Attestation().withPredicate(
                Attestation.Predicate(Lock.Predicate(List(Challenge().withRevealed(thresh)), 1), List.fill(3)(Proof()))
              )
            )
          )
        )
        .withOutputs(
          Seq(
            output.withValue(output.value.withLvl(Value.LVL(BigInt(-1)))),
            output.withValue(output.value.withLvl(Value.LVL(BigInt(100))))
          )
        )
      ctx = Context[F](testTx, 50, _ => None) // Tick should pass, height should fail
      res <- CredentiallerInterpreter
        .make[F](walletApi, MockWalletStateApi, MockMainKeyPair)
        .proveAndValidate(testTx, ctx)
    } yield res.swap

    val errsDisplay = vErrs.unsafeRunSync().toOption.get.map("> " + _.display).mkString("\n").trim
    assertNoDiff(
      errsDisplay,
      s"""
> Transaction has an output with a non-positive quantity value
> Transaction inputs cannot satisfy outputs
> Authorization failed. Causes:
- Proof does not satisfy proposition.
  Proposition: Threshold
    threshold: 3
    challenges:
    - And
      left: HeightRange
      right: Signature
        routine: ExtendedEd25519
        vk: GeMD3jTehpe52JDKVn8iiGYfjv7kJpaKKeu7ub1ayoB7W1eUwSVCWHkPdT9px9ne1oTesjkTVTQ9ZA5ub869wFwm9zhJPs1Z97
    - Not
        TickRange
    - Or
      left: Digest
        routine: Blake2b256
        8YmGaFtQ5WmMQ7i6uStCZF7T6N6B6w5CT8nChEsr5ZrA
      right: Locked
  Proof: Threshold
    responses:
    - And
      left: HeightRange
      right: Signature
        kg6S2aBmECwXiq6VF6XXUcCRgetPSGZc9tB8NJjFb9gMQNKe25DWUMLK1sgw7oH96qgu6D7qgy1uPfhGnj8Yjg7
    - Not
        TickRange
    - Or
      left: Digest
        input: zTuS2beK
        salt: 3x4JNf
      right: Locked""".stripMargin
    )
  }

}
