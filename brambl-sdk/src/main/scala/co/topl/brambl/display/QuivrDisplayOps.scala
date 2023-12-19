package co.topl.brambl.display

import co.topl.brambl.utils.Encoding
import quivr.models.{Proof, Proposition}

trait QuivrDisplayOps {

  implicit val propositionDisplay: DisplayOps[Proposition] = (p: Proposition) => displayProposition(p, 0)

  implicit val proofDisplay: DisplayOps[Proof] = (p: Proof) => displayProof(p, 0)

  private def displayProposition(p: Proposition, indent: Int, prefix: String = " "): String = p.value match {
    case Proposition.Value.Locked(_) => displayIndent("Locked", indent, prefix)
    case Proposition.Value.Digest(Proposition.Digest(routine, digest, _)) =>
      Seq(
        displayIndent("Digest", indent, prefix),
        displayIndent(s"routine: ${routine}", indent),
        displayIndent(s"${Encoding.encodeToBase58(digest.value.toByteArray)}", indent)
      ).mkString("\n")
    case Proposition.Value.DigitalSignature(Proposition.DigitalSignature(routine, vk, _)) =>
      Seq(
        displayIndent("Signature", indent, prefix),
        displayIndent(s"routine: ${routine}", indent),
        displayIndent(s"vk: ${Encoding.encodeToBase58(vk.toByteArray)}", indent)
      ).mkString("\n")
    case Proposition.Value.HeightRange(_) => displayIndent("HeightRange", indent, prefix)
    case Proposition.Value.TickRange(_)   => displayIndent("TickRange", indent, prefix)
    case Proposition.Value.ExactMatch(_)  => displayIndent("Exact", indent, prefix)
    case Proposition.Value.LessThan(_)    => displayIndent("LessThan", indent, prefix)
    case Proposition.Value.GreaterThan(_) => displayIndent("GreaterThan", indent, prefix)
    case Proposition.Value.EqualTo(_)     => displayIndent("EqualTo", indent, prefix)
    case Proposition.Value.Not(Proposition.Not(proposition, _)) =>
      Seq(
        displayIndent("Not", indent, prefix),
        displayProposition(proposition, indent + Indent)
      ).mkString("\n")
    case Proposition.Value.And(Proposition.And(left, right, _)) =>
      Seq(
        displayIndent("And", indent, prefix),
        displayProposition(left, indent + Indent, "left:"),
        displayProposition(right, indent + Indent, "right:")
      ).mkString("\n")
    case Proposition.Value.Or(Proposition.Or(left, right, _)) =>
      Seq(
        displayIndent("Or", indent, prefix),
        displayProposition(left, indent + Indent, "left:"),
        displayProposition(right, indent + Indent, "right:")
      ).mkString("\n")
    case Proposition.Value.Threshold(Proposition.Threshold(challenges, thresh, _)) =>
      Seq(
        displayIndent(s"Threshold ${thresh}", indent, prefix),
        displayIndent("challenges:", indent),
        // TODO: Not displaying properly
        displayIndent(challenges.map(r => displayProposition(r, 0, "-")).mkString("\n"), 0)
      ).mkString("\n")
    case Proposition.Value.Empty => displayIndent("EMPTY", indent, prefix) // Should never happen
    case _                       => displayIndent("UNKNOWN", indent, prefix) // Should never happen
  }

  private def displayProof(p: Proof, indent: Int, prefix: String = " "): String = p.value match {
    case Proof.Value.Empty     => displayIndent("EMPTY", indent, prefix)
    case Proof.Value.Locked(_) => displayIndent("Locked", indent, prefix)
    case Proof.Value.Digest(Proof.Digest(_, preimage, _)) =>
      Seq(
        displayIndent("Digest", indent, prefix),
        displayIndent(s"input: ${Encoding.encodeToBase58(preimage.input.toByteArray)}", indent),
        displayIndent(s"salt: ${Encoding.encodeToBase58(preimage.salt.toByteArray)}", indent)
      ).mkString("\n")
    case Proof.Value.DigitalSignature(Proof.DigitalSignature(_, witness, _)) =>
      Seq(
        displayIndent("Signature", indent, prefix),
        displayIndent(Encoding.encodeToBase58(witness.value.toByteArray), indent)
      ).mkString("\n")
    case Proof.Value.HeightRange(_) => displayIndent("HeightRange", indent, prefix)
    case Proof.Value.TickRange(_)   => displayIndent("TickRange", indent, prefix)
    case Proof.Value.ExactMatch(_)  => displayIndent("Exact", indent, prefix)
    case Proof.Value.LessThan(_)    => displayIndent("LessThan", indent, prefix)
    case Proof.Value.GreaterThan(_) => displayIndent("GreaterThan", indent, prefix)
    case Proof.Value.EqualTo(_)     => displayIndent("EqualTo", indent, prefix)
    case Proof.Value.Not(Proof.Not(_, proof, _)) =>
      Seq(
        displayIndent("Not", indent, prefix),
        displayProof(proof, indent + Indent)
      ).mkString("\n")
    case Proof.Value.And(Proof.And(_, left, right, _)) =>
      Seq(
        displayIndent("And", indent, prefix),
        displayProof(left, indent + Indent, "left:"),
        displayProof(right, indent + Indent, "right:")
      ).mkString("\n")
    case Proof.Value.Or(Proof.Or(_, left, right, _)) =>
      Seq(
        displayIndent("Or", indent, prefix),
        displayProof(left, indent + Indent, "left:"),
        displayProof(right, indent + Indent, "right:")
      ).mkString("\n")
    case Proof.Value.Threshold(Proof.Threshold(_, responses, _)) =>
      Seq(
        displayIndent("Threshold", indent, prefix),
        displayIndent("responses:", indent),
        displayIndent(responses.map(r => displayProof(r, indent + Indent, "-")).mkString("\n"), 0)
      ).mkString("\n")
    case _ => displayIndent("UNKNOWN", indent, prefix) // Should never happen
  }

}
