package co.topl.brambl.display

import co.topl.brambl.utils.Encoding
import co.topl.consensus.models.BlockId

trait BlockDisplayOps {

  implicit val blockIdDisplay: DisplayOps[BlockId] = (blockId: BlockId) =>
    Encoding.encodeToBase58(blockId.value.toByteArray())
}
