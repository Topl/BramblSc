package co.topl.brambl.playground.monitoring

import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.codecs.AddressCodecs
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.box.Lock
import co.topl.brambl.utils.Encoding
import org.bitcoins.core.protocol.BitcoinAddress
import quivr.models.VerificationKey

object Models {
  case class BridgeRequest(
                            hash: String,
                            bitcoinPk: String,
                            toplVk: VerificationKey
                          ) {
    def toMap: Map[String, String] = Map(
      "hash" -> hash,
      "bitcoinPk" -> bitcoinPk,
      "toplVk" -> Encoding.encodeToHex(toplVk.toByteArray)
    )
  }
  object BridgeRequest {
    def apply(hash: String, bitcoinPk: String, toplVk: String): BridgeRequest =
      BridgeRequest(
        hash,
        bitcoinPk,
        VerificationKey.parseFrom(Encoding.decodeFromHex(toplVk).toOption.get)
      )
  }

  case class BridgeResponse(
                             desc: String,
                             bitcoinAddress: BitcoinAddress,
                             toplLock: Lock,
                             toplAddress: LockAddress // Serves as a checksum for the toplLock
                           ) {
    def toJson: String = {
      val params = Map(
        "desc" -> desc,
        "toplLock" -> Encoding.encodeToHex(toplLock.toByteArray),
        "toplAddress" -> toplAddress.toBase58(),
        "bitcoinAddress" -> bitcoinAddress.value
      )
      params.map(p => s""""${p._1}":"${p._2}"""").mkString("{", ",", "}")
    }
  }

  object BridgeResponse {
    def apply(desc: String, toplLock: String, toplAddress: String, bitcoinAddress: String): BridgeResponse =
      BridgeResponse(
        desc,
        BitcoinAddress(bitcoinAddress),
        Lock.parseFrom(Encoding.decodeFromHex(toplLock).toOption.get),
        AddressCodecs.decodeAddress(toplAddress).toOption.get
      )

    def fromJson(json: String): BridgeResponse = {
      val map = json
        .stripPrefix("{\"")
        .stripSuffix("\"}")
        .split("\",\"")
        .map(_.split("\":\""))
        .map(arr => (arr(0), arr(1)))
        .toMap
      BridgeResponse(
        map("desc"),
        map("bitcoinAddress"),
        map("toplLock"),
        map("toplAddress")
      )
    }
  }
}
