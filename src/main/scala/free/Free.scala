package free

import scala.language.higherKinds

sealed trait Free[+F[_], A] {
  def fold[FF[x] >: F[x], B](pure: A => B)(impure: FF[B] => B)(implicit F: Functor[FF]): B = this match {
    case Return(a)   => pure(a)
    case Suspend(in) => impure(F.map(in.asInstanceOf[FF[Free[FF, A]]])(_.fold(pure)(impure)))
  }

  def map[FF[x] >: F[x], B](f: A => B)(implicit F: Functor[FF]): Free[FF, B] =
    flatMap[FF, B](a => Free.emit(f(a)))

  def flatMap[FF[x] >: F[x], B](f: A => Free[FF, B])(implicit F: Functor[FF]): Free[FF, B] =
    fold[FF, Free[FF, B]](f)(Free.suspend)
}

object Free {
  def emit[A](v: A): Free[Nothing, A] =
    Return(v)

  def suspend[F[_], A](s: F[Free[F, A]]): Free[F, A] =
    Suspend(s)
}

case class Return[A](v: A) extends Free[Nothing, A]
case class Suspend[F[_], A](in: F[Free[F, A]]) extends Free[F, A]
