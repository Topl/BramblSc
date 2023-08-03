package co.topl.quivr.example

trait Algebra[E[_]] {
  def b(boolean: Boolean): E[Boolean]
  def i(int:     Int): E[Int]

  def or(left:     E[Boolean], right: E[Boolean]): E[Boolean]
  def and(left:    E[Boolean], right: E[Boolean]): E[Boolean]
  def not(boolean: E[Boolean]): E[Boolean]
  def sum(left:    E[Int], right:     E[Int]): E[Int]
}

sealed abstract class SimpleExpr[A] {
  val value: A
}

object SimpleExpr {
  case class Bool(value: Boolean) extends SimpleExpr[Boolean]
  case class Num(value: Int) extends SimpleExpr[Int]

  case class Sum(left: SimpleExpr.Num, right: SimpleExpr.Num) extends SimpleExpr[Int] {
    val value: Int = left.value + right.value
  }

  case class Or(left: SimpleExpr.Bool, right: SimpleExpr.Bool) extends SimpleExpr[Boolean] {
    val value: Boolean = left.value || right.value
  }

  case class And(left: SimpleExpr.Bool, right: SimpleExpr.Bool) extends SimpleExpr[Boolean] {
    val value: Boolean = left.value && right.value
  }
  case class Not(bool: SimpleExpr.Bool) extends SimpleExpr[Boolean] { val value: Boolean = !bool.value }

  implicit val simpleExprAlg: Algebra[SimpleExpr] = new Algebra[SimpleExpr] {
    override def b(boolean: Boolean): SimpleExpr.Bool = SimpleExpr.Bool(boolean)
    override def i(int:     Int): SimpleExpr.Num = SimpleExpr.Num(int)

    override def or(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] =
      SimpleExpr.Bool(left.value || right.value)

    override def and(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] =
      SimpleExpr.Bool(left.value && right.value)
    override def not(boolean: SimpleExpr[Boolean]): SimpleExpr[Boolean] = SimpleExpr.Bool(!boolean.value)

    override def sum(left: SimpleExpr[Int], right: SimpleExpr[Int]): SimpleExpr[Int] =
      SimpleExpr.Num(left.value + right.value)
  }
}

object Sandbox {

  def main(arg: Array[String]): Unit = {
    import SimpleExpr._
    println(And(Bool(true), Bool(false)).value)
  }
}
