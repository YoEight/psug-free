package free

import mu.ExprInstr._

object ExprMuExample {
  def main(args: Array[String]) {
    val expr = // (4 / 2 * (10 - 5)) + 8
      add(mul(div(value(4), value(2)), sub(value(10), value(5))), value(8))

    val result = evaluate(expr)

    println(result)
  }
}
