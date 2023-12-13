package co.topl.brambl

package object display
    extends UtxoDisplayOps
    with ValueDisplayOps
    with StructDisplayOps
    with GroupDisplayOps
    with SeriesDisplayOps {
  val Sep = "\n-----------\n"

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
