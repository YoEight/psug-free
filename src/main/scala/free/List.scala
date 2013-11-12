package free

object List {
  type List[A] = Mu[({ type λ[α] = Fix[A, α] })#λ]

  def cons[A](head: A, tail: => List[A]): List[A] =
    Mu[({ type λ[α] = Fix[A, α] })#λ](Cons(head, () => tail))

  def nil[A]: List[A] = Mu[({ type λ[α] = Fix[A, α] })#λ](Empty)

  def singleton[A](a: A): List[A] = cons(a, nil)

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    xs.fold[List[B]] {
      case Empty       => nil
      case Cons(a, bs) => cons(f(a), bs())
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

  def show[A](xs: List[A]): String =
    xs.fold[String] {
      case Empty      => "Nil"
      case Cons(a, s) => a + " :: " + s()
    }
}
