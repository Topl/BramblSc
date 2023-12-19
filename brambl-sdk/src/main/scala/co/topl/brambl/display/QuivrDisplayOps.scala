package co.topl.brambl.display

import co.topl.brambl.utils.Encoding
import quivr.models.{Proof, Proposition}

trait QuivrDisplayOps {

  implicit val propositionDisplay: DisplayOps[Proposition] = (p: Proposition) => display(p, 0)

  implicit val proofDisplay: DisplayOps[Proof] = (p: Proof) => display(p, 0)
  private def display(txt: String, indent: Int, prefix: String = " "): String = " " * indent + prefix + " " + txt

  private def display(p: Proposition, indent: Int): String = p.value.value match {
    case Proposition.Value.Locked(_) => display("Locked", indent, "-")
    case Proposition.Value.Digest(Proposition.Digest(routine, digest, _)) =>
      Seq(
        display("Digest:", indent, "-"),
        display(s"routine: ${routine}", indent),
        display(s"${Encoding.encodeToBase58(digest.value.toByteArray)}", indent)
      ).mkString("\n")
    case Proposition.Value.DigitalSignature(Proposition.DigitalSignature(routine, vk, _)) =>
      Seq(
        display("Signature:", indent, "-"),
        display(s"routine: ${routine}", indent),
        display(s"vk: ${Encoding.encodeToBase58(vk.toByteArray)}", indent)
      ).mkString("\n")
    case Proposition.Value.HeightRange(_) => display("HeightRange", indent, "-")
    case Proposition.Value.TickRange(_)   => display("TickRange", indent, "-")
    case Proposition.Value.ExactMatch(_)  => display("Exact", indent, "-")
    case Proposition.Value.LessThan(_)    => display("LessThan", indent, "-")
    case Proposition.Value.GreaterThan(_) => display("GreaterThan", indent, "-")
    case Proposition.Value.EqualTo(_)     => display("EqualTo", indent, "-")
    case Proposition.Value.Not(Proposition.Not(proposition, _)) =>
      Seq(
        display("Not:", indent, "-"),
        display(proposition, indent + Indent)
      ).mkString("\n")
    case Proposition.Value.And(Proposition.And(left, right, _)) =>
      Seq(
        display("And:", indent, "-"),
        display("left:", indent),
        display(left, indent + Indent),
        display("right:", indent),
        display(right, indent + Indent)
      ).mkString("\n")
    case Proposition.Value.Or(Proposition.Or(left, right, _)) =>
      Seq(
        display("Or:", indent, "-"),
        display("left:", indent),
        display(left, indent + Indent),
        display("right:", indent),
        display(right, indent + Indent)
      ).mkString("\n")
    case Proposition.Value.Threshold(Proposition.Threshold(challenges, thresh, _)) =>
      Seq(
        display(s"Threshold: ${thresh}", indent, "-"),
        display("challenged:", indent),
        display(challenges.map(r => display(r, indent + Indent)).mkString("\n"), 0)
      ).mkString("\n")
    case Proposition.Value.Empty => display("EMPTY", indent, "-") // Should never happen
    case _                       => display("UNKNOWN", indent, "-") // Should never happen
  }

  private def display(p: Proof, indent: Int): String = p.value.value match {
    case Proof.Value.Empty     => display("EMPTY", indent, "-")
    case Proof.Value.Locked(_) => display("Locked", indent, "-")
    case Proof.Value.Digest(Proof.Digest(_, preimage, _)) =>
      Seq(
        display("Digest:", indent, "-"),
        display(s"input: ${Encoding.encodeToBase58(preimage.input.toByteArray)}", indent),
        display(s"salt: ${Encoding.encodeToBase58(preimage.salt.toByteArray)}", indent)
      ).mkString("\n")
    case Proof.Value.DigitalSignature(Proof.DigitalSignature(_, witness, _)) =>
      Seq(
        display("Signature:", indent, "-"),
        display(Encoding.encodeToBase58(witness.value.toByteArray), indent)
      ).mkString("\n")
    case Proof.Value.HeightRange(_) => display("HeightRange", indent, "-")
    case Proof.Value.TickRange(_)   => display("TickRange", indent, "-")
    case Proof.Value.ExactMatch(_)  => display("Exact", indent, "-")
    case Proof.Value.LessThan(_)    => display("LessThan", indent, "-")
    case Proof.Value.GreaterThan(_) => display("GreaterThan", indent, "-")
    case Proof.Value.EqualTo(_)     => display("EqualTo", indent, "-")
    case Proof.Value.Not(Proof.Not(_, proof, _)) =>
      Seq(
        display("Not:", indent, "-"),
        display(proof, indent + Indent)
      ).mkString("\n")
    case Proof.Value.And(Proof.And(_, left, right, _)) =>
      Seq(
        display("And:", indent, "-"),
        display("left:", indent),
        display(left, indent + Indent),
        display("right:", indent),
        display(right, indent + Indent)
      ).mkString("\n")
    case Proof.Value.Or(Proof.Or(_, left, right, _)) =>
      Seq(
        display("Or:", indent, "-"),
        display("left:", indent),
        display(left, indent + Indent),
        display("right:", indent),
        display(right, indent + Indent)
      ).mkString("\n")
    case Proof.Value.Threshold(Proof.Threshold(_, responses, _)) =>
      Seq(
        display("Threshold:", indent, "-"),
        display("responses:", indent),
        display(responses.map(r => display(r, indent + Indent)).mkString("\n"), 0)
      ).mkString("\n")
    case _ => display("UNKNOWN", indent, "-") // Should never happen
  }

}
