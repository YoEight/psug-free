package free
package mu

sealed trait ListInstr[+A, +B]
case class Cons[A, B](a: A, b: () => B) extends ListInstr[A, B]
case object Empty extends ListInstr[Nothing, Nothing]

object ListInstr {
  implicit def fixFunctor[E] =
    new Functor[({ type λ[α] = ListInstr[E, α] })#λ] {
      def map[A,B](fa: ListInstr[E, A])(f: A => B): ListInstr[E, B] = fa match {
        case Cons(a, b) => Cons(a, () => f(b()))
        case Empty      => Empty
      }
    }

  def cons[A](head: A, tail: => List[A]): List[A] =
    Mu[({ type λ[α] = ListInstr[A, α] })#λ](Cons(head, () => tail))

  def nil[A]: List[A] = Mu[({ type λ[α] = ListInstr[A, α] })#λ](Empty)

  def singleton[A](a: A): List[A] = cons(a, nil)

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    xs.fold[List[B]] {
      case Empty       => nil
      case Cons(a, bs) => cons(f(a), bs())
    }

  def append[A](xs: List[A], vs: List[A]): List[A] =
    xs.fold[List[A]] {
      case Empty       => vs
      case Cons(a, as) => cons(a, as())
    }

  def filter[A](xs: List[A])(f: A => Boolean): List[A] =
    xs.fold[List[A]] {
      case Empty       => nil
      case Cons(a, as) => if (f(a)) cons(a, as()) else as()
    }

  def find[A](xs: List[A])(f: A => Boolean): Option[A] =
    xs.fold[Option[A]] {
      case Empty      => None
      case Cons(a, o) => if (f(a)) Some(a) else o()
    }

  def foreach[A, U](xs: List[A])(f: A => U): Unit =
    xs.fold[Unit] {
      case Empty => ()
      case Cons(a, u) =>
        f(a)
        u()
    }

  def foldRight[A, B](xs: List[A], b: B)(f: (A, B) => B): B =
    xs.fold[B] {
      case Empty       => b
      case Cons(a, b1) => f(a, b1())
    }

  def foldLeft[A, B](xs: List[A], b: B)(f: (B, A) => B): B = {
    val res = xs.fold[B => B] {
      case Empty      => (b: B) => b
      case Cons(a, k) => (b: B) => k()(f(b, a))
    }

    res(b)
  }

  def show[A](xs: List[A]): String =
    xs.fold[String] {
      case Empty      => "Nil"
      case Cons(a, s) => a + " :: " + s()
    }
}
