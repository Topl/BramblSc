package co.topl.brambl.codecs

import co.topl.brambl.utils.EncodingError
import co.topl.brambl.models.LockAddress
import co.topl.brambl.utils.Encoding._
import co.topl.brambl.models.Evidence
import quivr.models.Digest
import com.google.protobuf.ByteString
import co.topl.brambl.models.LockId

object AddressCodecs {

  /**
   * Decodes a base58 string into a LockAddress.
   * @param address
   *  the base58 string to decode
   * @return
   *  the LockAddress
   */
  def decodeAddress(
    address: String
  ): Either[EncodingError, LockAddress] =
    for {
      byteArray <- decodeFromBase58Check(address)
      (network, ledgerAndId) = byteArray.splitAt(4)
      (ledger, id) = ledgerAndId.splitAt(4)
      lockAddress = LockAddress(
        network.head,
        ledger.head,
        LockId((ByteString.copyFrom(id)))
      )
    } yield lockAddress

  /**
   * Encodes a LockAddress into a base58 string.
   *
   * @param lockAddress
   *  the LockAddress to encode
   * @return
   *  the base58 string
   */
  def encodeAddress(lockAddress: LockAddress): String = {
    val networkByteArray = BigInt(lockAddress.network).toByteArray
    val network = Array.fill(4 - networkByteArray.length)(0.toByte) ++ networkByteArray
    val ledgerByteArray = BigInt(lockAddress.ledger).toByteArray
    val ledger = Array.fill(4 - ledgerByteArray.length)(0.toByte) ++ ledgerByteArray
    val id = lockAddress.id.value.toByteArray
    encodeToBase58Check(network ++ ledger ++ id)
  }

}
