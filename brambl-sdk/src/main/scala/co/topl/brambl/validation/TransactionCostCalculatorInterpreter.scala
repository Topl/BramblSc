package co.topl.brambl.validation

import cats.Applicative
import cats.implicits._
import co.topl.brambl.common.ContainsImmutable.instances.ioTransactionImmutable
import co.topl.brambl.models.box.Attestation
import co.topl.brambl.models.transaction._
import co.topl.brambl.validation.algebras.TransactionCostCalculator
import quivr.models.Proof

object TransactionCostCalculatorInterpreter {

  def make[F[_]: Applicative](transactionCostConfig: TransactionCostConfig): TransactionCostCalculator[F] =
    new TransactionCostCalculator[F] {

      import transactionCostConfig._
      import proofCostConfig._

      override def costOf(transaction: IoTransaction): F[Long] =
        (
          baseCost +
            transactionDataCost(transaction) +
            transaction.inputs.map(transactionInputCost).sum +
            transaction.outputs.map(transactionOutputCost).sum
        ).pure[F]

      /**
       * A Transaction consumes disk space and network bandwidth.  The bigger the transaction, the more it
       * costs to save and transmit.
       *
       * @param transaction the transaction to cost
       * @return a cost, represented as a Long
       */
      private def transactionDataCost(transaction: IoTransaction): Long = {
        val bytes = ioTransactionImmutable.immutableBytes(transaction).value
        bytes.size() * dataCostPerMB / 1024L / 1024L
      }

      /**
       * Calculates the cost of consuming a UTxO.  Consuming a UTxO clears up some space in the UTxO set (a good thing), but
       * verifying the Proof that consumes the UTxO costs some resources.
       *
       * @param input The input to cost
       * @return a cost, represented as a Long
       */
      private def transactionInputCost(input: SpentTransactionOutput): Long =
        inputCost +
        // Now add the cost of verifying the proofs
        (input.attestation.value match {
          case Attestation.Value.Predicate(p) =>
            p.responses.map(proofCost).sum
          case Attestation.Value.Image(p) =>
            p.responses.map(proofCost).sum
          case Attestation.Value.Commitment(p) =>
            p.responses.map(proofCost).sum
          case _ =>
            0L
        })

      /**
       * Proof verification has a CPU/memory cost associated with it.  Different proofs have different complexity.
       *
       * @param proof the proof to cost
       * @return a cost, represented as a Long
       */
      private def proofCost(proof: Proof): Long =
        proof.value match {
          case Proof.Value.Empty => emptyCost
          // Locked Proofs do not require txBind validation
          case _: Proof.Value.Locked           => lockedCost
          case _: Proof.Value.Digest           => txBindCost + digestCost
          case _: Proof.Value.DigitalSignature => txBindCost + digitalSignatureCost
          case _: Proof.Value.HeightRange      => txBindCost + heightRangeCost
          case _: Proof.Value.TickRange        => txBindCost + tickRangeCost
          case _: Proof.Value.ExactMatch       => txBindCost + exactMatchCost
          case _: Proof.Value.LessThan         => txBindCost + lessThanCost
          case _: Proof.Value.GreaterThan      => txBindCost + greaterThanCost
          case _: Proof.Value.EqualTo          => txBindCost + equalToCost
          case v: Proof.Value.Threshold        => txBindCost + thresholdCost + v.value.responses.map(proofCost).sum
          case v: Proof.Value.Not              => txBindCost + notCost + proofCost(v.value.proof)
          case v: Proof.Value.And => txBindCost + andCost + proofCost(v.value.left) + proofCost(v.value.right)
          case v: Proof.Value.Or  => txBindCost + orCost + proofCost(v.value.left) + proofCost(v.value.right)
        }

      /**
       * Calculates the cost of creating a UTxO.  Creating a UTxO adds some data to the chain which creates dust at minimum.
       *
       * @param output The output to cost
       * @return a cost, represented as a Long
       */
      private def transactionOutputCost(output: UnspentTransactionOutput): Long =
        outputCost
    }

}

/**
 * Configuration values for individual cost components
 * @param baseCost a base value to pad to the transaction cost
 * @param dataCostPerMB cost per megabyte of data of the transaction's immutable bytes
 * @param inputCost base cost per each consumed input (consuming an input is a good thing) (proof costs are added on)
 * @param outputCost base cost for each new output
 * @param proofCostConfig configuration values for individual proofs
 */
case class TransactionCostConfig(
  baseCost:        Long = 1,
  dataCostPerMB:   Long = 1024,
  inputCost:       Long = -1,
  outputCost:      Long = 5,
  proofCostConfig: ProofCostConfig = ProofCostConfig()
)

/**
 * Configuration values for individual proof cost components
 * @param txBindCost The cost to verify a TxBind (hash verification)
 * @param emptyCost The cost to verify an empty proof
 * @param lockedCost The cost to verify a locked proof
 * @param digestCost The cost to verify a digest/hash
 * @param digitalSignatureCost The cost to verify a digital signature (likely EC)
 * @param heightRangeCost The cost to verify a height range (probably cheap, statically provided value)
 * @param tickRangeCost The cost to verify a tick range (probably cheap, statically provided value)
 * @param exactMatchCost The cost to verify an exact match (probably cheap, lookup function)
 * @param lessThanCost The cost to verify a less than (probably cheap, lookup function)
 * @param greaterThanCost The cost to verify a greater than (probably cheap, lookup function)
 * @param equalToCost The cost to verify an equal to (probably cheap, lookup function)
 * @param thresholdCost The base cost to verify a threshold (recursive calls will be added)
 * @param andCost The base cost to verify an and (recursive calls will be added)
 * @param orCost The base cost to verify an or (recursive calls will be added)
 * @param notCost The base cost to verify a not (recursive call will be added)
 */
case class ProofCostConfig(
  txBindCost:           Long = 50,
  emptyCost:            Long = 1,
  lockedCost:           Long = 1,
  digestCost:           Long = 50,
  digitalSignatureCost: Long = 100,
  heightRangeCost:      Long = 5,
  tickRangeCost:        Long = 5,
  exactMatchCost:       Long = 10,
  lessThanCost:         Long = 10,
  greaterThanCost:      Long = 10,
  equalToCost:          Long = 10,
  thresholdCost:        Long = 1,
  andCost:              Long = 1,
  orCost:               Long = 1,
  notCost:              Long = 1
)
