package co.topl.brambl.playground

import cats.effect.unsafe.implicits.global
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.brambl.utils.Encoding
import com.google.protobuf.ByteString
import org.bitcoins.core.protocol.script.ScriptWitness
import quivr.models.Preimage

object SecretExtraction {

  def extractFromToplTx(txId: TransactionId, targetLockAddr: LockAddress): String = {
    val tx = bifrostQuery.fetchTransaction(txId).unsafeRunSync().get
    val attestation = tx.inputs
      .map(_.attestation.getPredicate)
      .find(att => targetLockAddr == txBuilder.lockAddress(Lock().withPredicate(att.lock)).unsafeRunSync())
      .get
    // The following is possible because we know the exact structure of the attestation
    val aliceProof = attestation.responses.head.getAnd
    val preimage = aliceProof.right.getDigest.preimage
    Encoding.encodeToHex(preimage.input.toByteArray ++ preimage.salt.toByteArray)
  }

  def extractFromBitcoinTx(proof: ScriptWitness): Preimage = {
    // the following is possible because we know the exact structure of the witness
    val preimageHex = proof.stack.last.toHex
    val secret = Encoding.decodeFromHex(preimageHex).toOption.get
    Preimage(ByteString.copyFrom(secret.take(SecretSize)), ByteString.copyFrom(secret.drop(SecretSize)))
  }
}
