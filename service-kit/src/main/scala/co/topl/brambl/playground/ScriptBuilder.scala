package co.topl.brambl.playground

import cats.Id
import co.topl.brambl.builders.locks.LockTemplate.PredicateTemplate
import co.topl.brambl.builders.locks.PropositionTemplate.{AndTemplate, DigestTemplate, SignatureTemplate}
import co.topl.brambl.models.box.Lock
import co.topl.brambl.utils.Encoding
import com.google.protobuf.ByteString
import org.bitcoins.core.protocol.script.RawScriptPubKey
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.script.control.{OP_ELSE, OP_ENDIF, OP_NOTIF}
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_CHECKSIGVERIFY, OP_SHA256}
import org.bitcoins.core.script.locktime.OP_CHECKSEQUENCEVERIFY
import org.bitcoins.core.script.splice.OP_SIZE
import org.bitcoins.core.util.BytesUtil
import quivr.models.{Digest, VerificationKey}

trait ScriptBuilder {
  def generateDescriptor(bridgeVk: String, hash:          String, userVk:   String): String
  def descToScriptPubKey(desc:     String): RawScriptPubKey
  def generateToplLock(aliceVk:    VerificationKey, hash: String, bridgeVk: VerificationKey): Lock
}

object ScriptBuilder {

  def sizeOf(toPush: String): ScriptConstant = {
    val size = toPush.length / 2
    ScriptConstant("%02x".format(size))
  }

  def extractData(fullStr: String, token: String): String = {
    val start = fullStr.indexOf(token) + token.length
    val end = fullStr.indexOf(")", start)
    fullStr.substring(start, end)
  }

  object PegIn extends ScriptBuilder {

    // andor(pk(BridgeVk),sha256(H),and_v(v:pk(AliceVk),older(1000)))
    override def generateDescriptor(bridgeVk: String, hash: String, userVk: String): String = {
      val descriptor = s"wsh(andor(pk($bridgeVk),sha256($hash),and_v(v:pk($userVk),older(1000))))"
      // Bridge adds the checksum
      handleCall(rpcCli.getCanonicalDescriptor("bridge", descriptor)).get
    }

    /**
     * Only works with our specific descriptor
     * We only support 33byte public keys in hex
     * per: BIP-143
     * Each public key passed to a sigop inside version 0 witness program must be a compressed key:
     * the first byte MUST be either 0x02 or 0x03, and the size MUST be 33 bytes.
     *
     * wsh(andor(pk(BridgeVk),sha256(H),and_v(v:pk(AliceVk),older(1000))))
     *
     * <BridgeVk> OP_CHECKSIG OP_NOTIF
     * <AliceVk> OP_CHECKSIGVERIFY <e803> OP_CHECKSEQUENCEVERIFY
     * OP_ELSE
     * OP_SIZE <20> OP_EQUALVERIFY OP_SHA256 <H> OP_EQUAL
     * OP_ENDIF
     */
    def descToScriptPubKey(desc: String): RawScriptPubKey = {
      val bridgeVk = extractData(desc, "andor(pk(")

      val secret = extractData(desc, "sha256(")
      val secretSize = "%02x".format(32)

      val userVk = extractData(desc, "v:pk(")

      val seqLockTime = BytesUtil.flipEndianness("%02x".format(1000))

      val scriptTokens = Seq(
        sizeOf(bridgeVk), // op codes 1-75 indicates the number of bytes to push
        ScriptConstant(bridgeVk),
        OP_CHECKSIG,
        OP_NOTIF,
        sizeOf(userVk),
        ScriptConstant(userVk),
        OP_CHECKSIGVERIFY,
        sizeOf(seqLockTime),
        ScriptConstant(seqLockTime),
        OP_CHECKSEQUENCEVERIFY,
        OP_ELSE,
        OP_SIZE,
        sizeOf(secretSize),
        ScriptConstant(secretSize),
        OP_EQUALVERIFY,
        OP_SHA256,
        sizeOf(secret),
        ScriptConstant(secret),
        OP_EQUAL,
        OP_ENDIF
      )

      println("script: " + scriptTokens.map(_.hex).mkString(""))
      RawScriptPubKey(scriptTokens)
    }

    override def generateToplLock(aliceVk: VerificationKey, hash: String, bridgeVk: VerificationKey): Lock = {
      val hashBytes = Encoding.decodeFromHex(hash).toOption.get
      PredicateTemplate[Id](
        Seq(
          // Alice case
          AndTemplate(
            SignatureTemplate("ExtendedEd25519", 0),
            DigestTemplate("Sha256", Digest(ByteString.copyFrom(hashBytes)))
          ),
          // Bridge case
          SignatureTemplate("ExtendedEd25519", 1)
        ),
        1
      ).build(List(aliceVk, bridgeVk)).toOption.get
    }

  }

  object PegOut extends ScriptBuilder {

    // andor(pk(Alice),sha256(H),pk(Bridge))
    override def generateDescriptor(bridgeVk: String, hash: String, userVk: String): String = {
      val descriptor = s"wsh(andor(pk($userVk),sha256($hash),pk($bridgeVk)))"
      // Bridge adds the checksum
      handleCall(rpcCli.getCanonicalDescriptor("bridge", descriptor)).get
    }

    /**
     * Only works with our specific descriptor
     * We only support 33byte public keys in hex
     * per: BIP-143
     * Each public key passed to a sigop inside version 0 witness program must be a compressed key:
     * the first byte MUST be either 0x02 or 0x03, and the size MUST be 33 bytes.
     *
     * wsh(andor(pk(Alice),sha256(H),pk(Bridge)))
     *
     * <Alice> OP_CHECKSIG OP_NOTIF
     * <Bridge> OP_CHECKSIG
     * OP_ELSE
     * OP_SIZE <20> OP_EQUALVERIFY OP_SHA256 <H> OP_EQUAL
     * OP_ENDIF
     */
    def descToScriptPubKey(desc: String): RawScriptPubKey = {
      val userVk = extractData(desc, "andor(pk(")

      val secret = extractData(desc, "sha256(")
      val secretSize = "%02x".format(32)

      val bridgeVk = extractData(desc, "),pk(")

      val scriptTokens = Seq(
        sizeOf(userVk), // op codes 1-75 indicates the number of bytes to push
        ScriptConstant(userVk),
        OP_CHECKSIG,
        OP_NOTIF,
        sizeOf(bridgeVk),
        ScriptConstant(bridgeVk),
        OP_CHECKSIG,
        OP_ELSE,
        OP_SIZE,
        sizeOf(secretSize),
        ScriptConstant(secretSize),
        OP_EQUALVERIFY,
        OP_SHA256,
        sizeOf(secret),
        ScriptConstant(secret),
        OP_EQUAL,
        OP_ENDIF
      )

      println("script: " + scriptTokens.map(_.hex).mkString(""))
      RawScriptPubKey(scriptTokens)
    }

    override def generateToplLock(aliceVk: VerificationKey, hash: String, bridgeVk: VerificationKey): Lock = {
      val hashBytes = Encoding.decodeFromHex(hash).toOption.get
      PredicateTemplate[Id](
        Seq(
          // Bridge case
          AndTemplate(
            SignatureTemplate("ExtendedEd25519", 1),
            DigestTemplate("Sha256", Digest(ByteString.copyFrom(hashBytes)))
          ),
          // Alice case
          SignatureTemplate("ExtendedEd25519", 0)
        ),
        1
      ).build(List(aliceVk, bridgeVk)).toOption.get
    }
  }

}
