package co.topl.brambl.playground

import org.bitcoins.core.protocol.script.{NonStandardScriptSignature, ScriptSignature}
import org.bitcoins.core.script.constant.{OP_0, ScriptConstant}
import org.bitcoins.crypto.ECDigitalSignature

/**
 * Any witness stack items before the witnessScript are used as the input stack for script evaluation.
 * The input stack is not interpreted as script.
 * For example, there is no need to use a 0x4c (OP_PUSHDATA1) to “push” a big item.
 */
trait SignatureBuilder {
  def getReclaimSig(txSig: ECDigitalSignature): ScriptSignature
  def getClaimSig(txSig:   ECDigitalSignature, preimageHex: String): ScriptSignature
}

object SignatureBuilder {

  object PegIn extends SignatureBuilder {

    // user reclaims
    override def getReclaimSig(txSig: ECDigitalSignature): ScriptSignature = NonStandardScriptSignature.fromAsm(
      Seq(
        ScriptConstant(txSig.hex), // To satisfy the user's vk
        OP_0 // to fail the bridge's vk
      )
    )

    // bridge claims
    override def getClaimSig(txSig: ECDigitalSignature, preimageHex: String): ScriptSignature =
      NonStandardScriptSignature.fromAsm(
        Seq(
          ScriptConstant(preimageHex), // To satisfy the hash
          ScriptConstant(txSig.hex) // To satisfy the bridge's vk
        )
      )
  }

  object PegOut extends SignatureBuilder {

    // bridge reclaims
    override def getReclaimSig(txSig: ECDigitalSignature): ScriptSignature = NonStandardScriptSignature.fromAsm(
      Seq(
        ScriptConstant(txSig.hex), // To satisfy the bridge's vk
        OP_0 // to fail the user's vk
      )
    )

    // user claims
    override def getClaimSig(txSig: ECDigitalSignature, preimageHex: String): ScriptSignature =
      NonStandardScriptSignature.fromAsm(
        Seq(
          ScriptConstant(preimageHex), // To satisfy the hash
          ScriptConstant(txSig.hex) // To satisfy the user's vk
        )
      )
  }
}
