package free

package object mu {
  type Expr    = Mu[ExprInstr]
  type Console = Mu[ConsoleInstr]
  type List[A] = Mu[({ type λ[α] = ListInstr[A, α] })#λ]
}
