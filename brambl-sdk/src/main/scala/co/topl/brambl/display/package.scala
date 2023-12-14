package co.topl.brambl

package object display
    extends UtxoDisplayOps
    with StxoDisplayOps
    with ValueDisplayOps
    with StructDisplayOps
    with AssetDisplayOps
    with GroupDisplayOps
    with SeriesDisplayOps
    with TransactionDisplayOps {

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
