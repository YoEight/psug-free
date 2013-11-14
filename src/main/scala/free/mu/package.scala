package free

package object mu {
  type Expr    = Mu[ExprInstr]
  type Console = Mu[ConsoleInstr]
}
