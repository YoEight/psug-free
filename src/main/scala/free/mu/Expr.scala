package free
package mu

sealed trait ExprInstr[+A]
case class Val(i: Double) extends ExprInstr[Nothing]
case class Add[A](l: A, r: A) extends ExprInstr[A]
case class Mul[A](l: A, r: A) extends ExprInstr[A]
case class Div[A](l: A, r: A) extends ExprInstr[A]
case class Sub[A](l: A, r: A) extends ExprInstr[A]

object ExprInstr {
  implicit val exprInstr = new Functor[ExprInstr] {
    def map[A, B](fa: ExprInstr[A])(f: A => B): ExprInstr[B] = fa match {
      case Val(i)    => Val(i)
      case Add(l, r) => Add(f(l), f(r))
      case Mul(l, r) => Mul(f(l), f(r))
      case Div(l, r) => Div(f(l), f(r))
      case Sub(l, r) => Sub(f(l), f(r))
    }
  }

  def value(i: Double): Expr = Mu[ExprInstr](Val(i))

  def add(l: Expr, r: Expr): Expr = Mu(Add(l, r))

  def mul(l: Expr, r: Expr): Expr = Mu(Mul(l, r))

  def div(l: Expr, r: Expr): Expr = Mu(Div(l, r))

  def sub(l: Expr, r: Expr): Expr = Mu(Sub(l, r))

  def evaluate(expr: Expr): Double =
    expr.fold[Double] {
      case Val(i)    => i
      case Add(l, r) => l + r
      case Mul(l, r) => l * r
      case Div(l, r) => l / r
      case Sub(l, r) => l - r
    }
}
