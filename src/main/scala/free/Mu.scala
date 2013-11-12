package free

import scala.language.higherKinds

case class Mu[F[_]](in: F[Mu[F]]) {
  def fold[B](f: F[B] => B)(implicit F: Functor[F]): B =
    f(F.map(in)(_.fold(f)))
}
