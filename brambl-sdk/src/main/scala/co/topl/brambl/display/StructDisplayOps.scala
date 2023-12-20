package co.topl.brambl.display

import com.google.protobuf.struct.{Struct, Value}
import com.google.protobuf.struct.Value.Kind.{
  BoolValue,
  Empty,
  ListValue,
  NullValue,
  NumberValue,
  StringValue,
  StructValue
}

trait StructDisplayOps {
  private val Indent = 2
  private val InitialIndent = Indent

  implicit val structDisplay: DisplayOps[Struct] = (struct: Struct) =>
    " " * InitialIndent + display(struct, InitialIndent)

  private def display(struct: Struct, indent: Int): String =
    struct.fields.view.keys
      .map({ key =>
        struct.fields(key).kind match {
          case StructValue(s) =>
            s"$key:\n" + " " * (indent + Indent) + s"${display(s, indent + Indent)}"
          case ListValue(l) =>
            s"$key:\n" + l.values
              .map(s => " " * (indent + Indent) + s"-${display(s, 0)}")
              .mkString("\n")
          case _ => s"$key: ${display(struct.fields(key), indent)}"
        }
      })
      .mkString("\n" + " " * indent)

  private def display(v: Value, indent: Int): String = v match {
    case Value(NullValue(_), _)   => "null"
    case Value(Empty, _)          => "empty"
    case Value(BoolValue(b), _)   => b.toString()
    case Value(NumberValue(n), _) => n.toString()
    case Value(StringValue(s), _) => s
    case Value(ListValue(l), _) =>
      l.values.map(s => " " * indent + s"- ${display(s, 0)}").mkString("\n")
    case Value(StructValue(s), _) =>
      " " * indent + display(s, indent)
  }
}
