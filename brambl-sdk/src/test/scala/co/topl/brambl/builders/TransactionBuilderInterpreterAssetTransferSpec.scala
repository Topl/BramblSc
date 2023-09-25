package co.topl.brambl.builders

import cats.implicits.catsSyntaxOptionId
import co.topl.brambl.builders.TransactionBuilderApi.UnableToBuildTransaction
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.{
  assetAsBoxVal,
  bigIntAsInt128,
  groupAsBoxVal,
  groupPolicyAsGroupPolicySyntaxOps,
  int128AsBigInt,
  ioTransactionAsTransactionSyntaxOps,
  seriesAsBoxVal,
  seriesPolicyAsSeriesPolicySyntaxOps,
  valueToQuantitySyntaxOps,
  GroupAndSeriesFungible,
  GroupFungible,
  SeriesFungible
}

class TransactionBuilderInterpreterAssetTransferSpec extends TransactionBuilderInterpreterSpecBase {

  test("buildAssetTransferTransaction > underlying error fails (unsupported token type)") {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        mockSeriesPolicy.quantityDescriptor
      ),
      mockTxos :+ valToTxo(Value.defaultInstance.withTopl(Value.TOPL(quantity))),
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(testTx, Left(UnableToBuildTransaction(Seq(UserInputError(s"Invalid value type")))))
  }

  test("buildAssetTransferTransaction > quantity to transfer is non positive") {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        mockSeriesPolicy.quantityDescriptor
      ),
      mockTxos,
      inPredicateLockFull,
      0,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(testTx, Left(UnableToBuildTransaction(Seq(UserInputError(s"quantity to transfer must be positive")))))
  }

  test("buildAssetTransferTransaction > a txo isnt tied to lockPredicateFrom") {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        mockSeriesPolicy.quantityDescriptor
      ),
      mockTxos :+ valToTxo(value, trivialLockAddress),
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(
      testTx,
      Left(UnableToBuildTransaction(Seq(UserInputError(s"every lock does not correspond to fromLockAddr"))))
    )
  }

  test("buildAssetTransferTransaction > non sufficient funds") {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        mockSeriesPolicy.quantityDescriptor
      ),
      mockTxos,
      inPredicateLockFull,
      4,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError(s"All tokens selected to transfer do not have enough funds to transfer"))
        )
      )
    )
  }

  test("buildAssetTransferTransaction > fee not satisfied") {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        mockSeriesPolicy.quantityDescriptor
      ),
      mockTxos,
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      3
    )
    assertEquals(
      testTx,
      Left(
        UnableToBuildTransaction(
          Seq(UserInputError(s"Not enough LVLs in input to satisfy fee"))
        )
      )
    )
  }

  test("buildAssetTransferTransaction > [complex, GROUP_AND_SERIES] duplicate inputs are merged and split correctly") {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        mockSeriesPolicy.quantityDescriptor
      ),
      mockTxos,
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, assetGroupSeries), // recipient
          UnspentTransactionOutput(trivialLockAddress, assetGroupSeries),
          UnspentTransactionOutput(trivialLockAddress, value),
          UnspentTransactionOutput(trivialLockAddress, groupValue.copy(groupValue.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            groupValue.copy(groupValue.getGroup.withGroupId(mockGroupPolicyAlt.computeId))
          ),
          UnspentTransactionOutput(trivialLockAddress, seriesValue.copy(seriesValue.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            seriesValue.copy(seriesValue.getSeries.withSeriesId(mockSeriesPolicyAlt.computeId))
          ),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroupSeries.copy(
              assetGroupSeries.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          ),
          UnspentTransactionOutput(trivialLockAddress, assetGroup.copy(assetGroup.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroup.copy(
              assetGroup.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          ),
          UnspentTransactionOutput(trivialLockAddress, assetSeries.copy(assetSeries.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetSeries.copy(
              assetSeries.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          )
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildAssetTransferTransaction > [complex, GROUP] duplicate inputs are merged and split correctly") {
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId.value,
        mockSeriesPolicy.quantityDescriptor
      ),
      mockTxos,
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, assetGroup), // recipient
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2))
          ),
          UnspentTransactionOutput(trivialLockAddress, value),
          UnspentTransactionOutput(trivialLockAddress, groupValue.copy(groupValue.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            groupValue.copy(groupValue.getGroup.withGroupId(mockGroupPolicyAlt.computeId))
          ),
          UnspentTransactionOutput(trivialLockAddress, seriesValue.copy(seriesValue.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            seriesValue.copy(seriesValue.getSeries.withSeriesId(mockSeriesPolicyAlt.computeId))
          ),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroupSeries.copy(
              assetGroupSeries.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          ),
          UnspentTransactionOutput(trivialLockAddress, assetGroup),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroup.copy(
              assetGroup.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          ),
          UnspentTransactionOutput(trivialLockAddress, assetSeries.copy(assetSeries.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetSeries.copy(
              assetSeries.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          )
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildAssetTransferTransaction > [complex, SERIES] duplicate inputs are merged and split correctly") {
    val testTx = txBuilder.buildAssetTransferTransaction(
      SeriesFungible(
        mockSeriesPolicy.computeId,
        mockGroupPolicy.computeId.value,
        mockSeriesPolicy.quantityDescriptor
      ),
      mockTxos,
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      1
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(mockTxos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(
        List(
          UnspentTransactionOutput(inLockFullAddress, assetSeries), // recipient
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroupSeries.copy(assetGroupSeries.value.setQuantity(quantity * 2))
          ),
          UnspentTransactionOutput(trivialLockAddress, value),
          UnspentTransactionOutput(trivialLockAddress, groupValue.copy(groupValue.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            groupValue.copy(groupValue.getGroup.withGroupId(mockGroupPolicyAlt.computeId))
          ),
          UnspentTransactionOutput(trivialLockAddress, seriesValue.copy(seriesValue.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            seriesValue.copy(seriesValue.getSeries.withSeriesId(mockSeriesPolicyAlt.computeId))
          ),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroupSeries.copy(
              assetGroupSeries.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          ),
          UnspentTransactionOutput(trivialLockAddress, assetGroup.copy(assetGroup.value.setQuantity(quantity * 2))),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetGroup.copy(
              assetGroup.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          ),
          UnspentTransactionOutput(trivialLockAddress, assetSeries),
          UnspentTransactionOutput(
            trivialLockAddress,
            assetSeries.copy(
              assetSeries.getAsset.copy(mockGroupPolicyAlt.computeId.some, mockSeriesPolicyAlt.computeId.some)
            )
          )
        )
      )
    assertEquals(
      sortedTx(testTx.toOption.get).computeId,
      sortedTx(expectedTx).computeId
    )
  }

  test("buildAssetTransferTransaction > [simplest case] no change, only 1 output") {
    val txos = Seq(valToTxo(assetGroupSeries))
    val testTx = txBuilder.buildAssetTransferTransaction(
      GroupAndSeriesFungible(
        mockGroupPolicy.computeId,
        mockSeriesPolicy.computeId,
        mockSeriesPolicy.quantityDescriptor
      ),
      txos,
      inPredicateLockFull,
      1,
      inLockFullAddress,
      trivialLockAddress,
      0
    )
    val expectedTx = IoTransaction.defaultInstance
      .withDatum(txDatum)
      .withInputs(txos.map(txo => SpentTransactionOutput(txo.outputAddress, attFull, txo.transactionOutput.value)))
      .withOutputs(List(UnspentTransactionOutput(inLockFullAddress, assetGroupSeries)))
    assertEquals(testTx.toOption.get.computeId, expectedTx.computeId)
  }

  // TODO: Test for different quantity descriptors for transfer type and also the TXOs
}
