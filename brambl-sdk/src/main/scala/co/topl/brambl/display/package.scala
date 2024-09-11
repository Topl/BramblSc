package co.topl.brambl

package object display
    extends UtxoDisplayOps
    with StxoDisplayOps
    with ValueDisplayOps
    with StructDisplayOps
    with AssetDisplayOps
    with GroupDisplayOps
    with SeriesDisplayOps
    with TransactionDisplayOps
    with QuivrDisplayOps
    with TypeIdentifierDisplayOps
    with ValidationErrorDisplayOps
    with BlockDisplayOps {

  val LabelLength = 27
  val Indent = 2

  def displayIndent(txt: String, indent: Int, prefix: String = " "): String = " " * indent + prefix + " " + txt

  def padLabel(label: String): String = {
    val padding = " " * (LabelLength - label.length).max(0)
    s"${label}${padding}: "
  }

  trait DisplayOps[T] {
    def display(t: T): String
  }

  object DisplayOps {
    def apply[T](implicit ev: DisplayOps[T]): DisplayOps[T] = ev

    implicit class DisplayTOps[T: DisplayOps](t: T) {
      def display: String = DisplayOps[T].display(t)
    }
  }
}
