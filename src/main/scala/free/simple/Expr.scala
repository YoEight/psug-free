package free.simple

object Expr {
  sealed trait Expr
  case class Val(i: Double) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Mul(l: Expr, r: Expr) extends Expr
  case class Div(l: Expr, r: Expr) extends Expr
  case class Sub(l: Expr, r: Expr) extends Expr

  def evaluate(expr: Expr): Double = expr match {
    case Val(i)    => i
    case Add(l, r) => evaluate(l) + evaluate(r)
    case Mul(l, r) => evaluate(l) * evaluate(r)
    case Div(l, r) => evaluate(l) / evaluate(r)
    case Sub(l, r) => evaluate(l) - evaluate(r)
  }
}
