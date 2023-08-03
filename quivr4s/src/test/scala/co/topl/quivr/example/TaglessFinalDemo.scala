package co.topl.quivr.example

/**
 * This example follows from the tutorial from RockTheJVM posted here
 * https://blog.rockthejvm.com/tagless-final/
 */

object TaglessFinalDemo {

  trait Algebra[E[_]] {
    def b(boolean: Boolean): E[Boolean]

    def i(int: Int): E[Int]

    def d(double: Double): E[Double]

    def or(left: E[Boolean], right: E[Boolean]): E[Boolean]

    def and(left: E[Boolean], right: E[Boolean]): E[Boolean]

    def sum(left: E[Int], right: E[Int]): E[Int]

    def multDouble(a: E[Double], b: E[Double]): E[Double]
  }

  case class SimpleExpr[A](value: A)

  implicit val simpleExprAlg: Algebra[SimpleExpr] = new Algebra[SimpleExpr] {
    override def b(boolean: Boolean) = SimpleExpr(boolean)

    override def i(int: Int) = SimpleExpr(int)

    override def d(double: Double) = SimpleExpr(double)

    override def or(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]) = SimpleExpr(left.value || right.value)

    override def and(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]) = SimpleExpr(left.value && right.value)

    override def sum(left: SimpleExpr[Int], right: SimpleExpr[Int]) = SimpleExpr(left.value + right.value)

    override def multDouble(a: SimpleExpr[Double], b: SimpleExpr[Double]) = SimpleExpr(a.value * b.value * 34d)
  }

}
