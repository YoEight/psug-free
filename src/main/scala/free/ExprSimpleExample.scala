package free

import simple.Expr._

object ExprSimpleExample {
  def main(args: Array[String]) {
    val expr = // (4 / 2 * (10 - 5)) + 8
      Add(Mul(Div(Val(4), Val(2)), Sub(Val(10), Val(5))), Val(8))

    val result = evaluate(expr)

    println(result)
  }
}
