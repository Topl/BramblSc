package co.topl.brambl.routines.digests

import co.topl.crypto.hash.Blake2b256
import com.google.protobuf.ByteString
import quivr.models.{Digest, Preimage}

object Blake2b256Digest extends Hash {
  override val routine: String = "blake2b256"

  override def hash(preimage: Preimage): Digest = {
    val digest = (new Blake2b256).hash(preimage.input.toByteArray ++ preimage.salt.toByteArray)
    Digest().withDigest32(
      Digest.Digest32(ByteString.copyFrom(digest))
    )
  }
}
