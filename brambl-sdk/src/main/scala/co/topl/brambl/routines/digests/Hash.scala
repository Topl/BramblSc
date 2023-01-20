package co.topl.brambl.routines.digests

import co.topl.brambl.routines.Routine
import quivr.models._

trait Hash extends Routine {
  def hash(preimage: Preimage): Digest
}
