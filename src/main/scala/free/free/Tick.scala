package free
package free

sealed trait Tick[A]
case class GetTick[A](k: Int => A) extends Tick[A]
case class SetTick[A](v: Int, a: A) extends Tick[A]

object Tick {
  implicit val tickFunctor = new Functor[Tick] {
    def map[A,B](fa: Tick[A])(f: A => B): Tick[B] = fa match {
      case GetTick(k)    => GetTick(i => f(k(i)))
      case SetTick(i, a) => SetTick(i, f(a))
    }
  }

  def getTick: Free[Tick, Int] =
    Free.suspend(GetTick(Free.emit))

  def setTick(i: Int): Free[Tick, Unit] =
    Free.suspend(SetTick(i, Free.emit()))

  def modifiyTick(k: Int => Int): Free[Tick, Unit] =
    for {
      i <- getTick
      _ <- setTick(k(i))
    } yield ()

  def interpret[A](instr: Free[Tick, A]): (Int, A) = {
    val res = instr.fold[Tick, Int => (Int, A)](a => (i: Int) => (i, a)){
      case GetTick(k)    => (i: Int) => k(i)(i)
      case SetTick(i, k) => (_: Int) => k(i)
    }

    res(1)
  }
}
