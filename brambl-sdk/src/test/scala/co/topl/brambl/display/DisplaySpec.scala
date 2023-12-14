package co.topl.brambl.display

import co.topl.brambl.MockHelpers
import co.topl.brambl.display.DisplayOps.DisplayTOps
import co.topl.brambl.models.box.AssetMintingStatement
import co.topl.brambl.models.transaction.Schedule
import co.topl.brambl.models.{Datum, Event}
import co.topl.brambl.syntax.longAsInt128
import com.google.protobuf.ByteString
import quivr.models.SmallData

import scala.language.implicitConversions

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
Commitment                 : No commitment
Ephemeral-Metadata         : \nNo ephemeral metadata
Value                      : 1

Datum
=====
Value                      : KJHK1EAZuVA
""".trim()
    )
  }
}
