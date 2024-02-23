package co.topl.brambl.playground

import co.topl.brambl.models.box.Attestation
import co.topl.brambl.utils.Encoding
import com.google.protobuf.ByteString
import org.bitcoins.core.protocol.script.ScriptWitness
import quivr.models.Preimage

object SecretExtraction {

  def extractFromToplTx(proof: Attestation): String = {
    // The following is possible because we know the exact structure of the attestation
    val attestation = proof.getPredicate
    val preimage = attestation.responses.head.getAnd.right.getDigest.preimage
    Encoding.encodeToHex(preimage.input.toByteArray ++ preimage.salt.toByteArray)
  }

  def extractFromBitcoinTx(proof: ScriptWitness): Preimage = {
    // the following is possible because we know the exact structure of the witness
    val preimageHex = proof.stack.last.toHex
    val secret = Encoding.decodeFromHex(preimageHex).toOption.get
    Preimage(ByteString.copyFrom(secret.take(SecretSize)), ByteString.copyFrom(secret.drop(SecretSize)))
  }
}
