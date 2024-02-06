package co.topl.brambl.playground

import cats.effect.unsafe.implicits.global
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.brambl.playground.ScriptBuilder.PegOut
import co.topl.brambl.utils.Encoding
import com.google.protobuf.ByteString
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.crypto.DoubleSha256DigestBE
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

  // TODO: If we need to debug, check the order of head/tail here
  def extractFromBitcoinTx(txId: DoubleSha256DigestBE, desc: String): Preimage = {
    val tx = handleCall(rpcCli.getRawTransactionRaw(txId)).get.asInstanceOf[WitnessTransaction]
    val scriptInner = PegOut.descToScriptPubKey(desc)
    val aliceProof = tx.witness.witnesses.find(wit => wit.stack.head == scriptInner.asmBytes).get
    val signature = ScriptSignature.fromBytes(aliceProof.stack.last)
    // We know the exact structure of the signature since we generated it. It was from PegOut.getClaimSig
    val secretHex = signature.asm.head.bytes.toHex
    val secret = Encoding.decodeFromHex(secretHex).toOption.get // Should have
    Preimage(ByteString.copyFrom(secret.take(SecretSize)), ByteString.copyFrom(secret.drop(SecretSize)))
  }
}
