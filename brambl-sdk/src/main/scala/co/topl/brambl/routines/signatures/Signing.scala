package co.topl.brambl.routines.signatures

import co.topl.brambl.routines.Routine
import quivr.models._

trait Signing extends Routine{
  def createKeyPair(seed: Array[Byte]): KeyPair
  def sign(sk:            SigningKey, msg: SignableBytes): Witness
}
