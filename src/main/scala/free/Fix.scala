package free

sealed trait Fix[+A, +B] {
  def map[AA >: A, C](f: B => C): Fix[AA, C] = this match {
    case Cons(a, b) => Cons(a, () => f(b()))
    case Empty      => Empty
  }
}

case class Cons[A, B](a: A, b: () => B) extends Fix[A, B]
case object Empty extends Fix[Nothing, Nothing]

object Fix {
  implicit def fixFunctor[E] = new Functor[({ type λ[α] = Fix[E, α] })#λ] {
    def map[A,B](fa: Fix[E, A])(f: A => B): Fix[E, B] = fa map f
  }
}
