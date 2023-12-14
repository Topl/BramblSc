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
    with BlockDisplayOps {

  val LabelLength = 14

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
