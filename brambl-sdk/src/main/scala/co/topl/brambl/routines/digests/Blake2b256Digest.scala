package co.topl.brambl.routines.digests

import co.topl.crypto.hash.Blake2b256
import com.google.protobuf.ByteString
import quivr.models.{Digest, Preimage}
import scodec.bits.ByteVector

object Blake2b256Digest extends Hash {
  override val routine: String = "blake2b256"

  override def hash(preimage: Preimage): Digest = {
    val digest = (new Blake2b256).hash(ByteVector(preimage.toByteArray ++ preimage.toByteArray))
    Digest().withDigest32(
      Digest.Digest32(ByteString.copyFrom(digest.toArray))
    )
  }
}
