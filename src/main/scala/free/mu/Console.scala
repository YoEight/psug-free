package free
package mu

sealed trait ConsoleInstr[+A]
case class GetLine[A](k: String => A) extends ConsoleInstr[A]
case class PutLine[A](s: String, next: A) extends ConsoleInstr[A]
case object Stop extends ConsoleInstr[Nothing]

object ConsoleInstr {
  implicit val consoleInstrFunctor = new Functor[ConsoleInstr] {
    def map[A, B](fa: ConsoleInstr[A])(f: A => B): ConsoleInstr[B] = fa match {
      case GetLine(k)    => GetLine(s => f(k(s)))
      case PutLine(s, a) => PutLine(s, f(a))
      case Stop          => Stop
    }
  }

  def getLine(k: String => Console): Console = Mu(GetLine(k))

  def putLine(s: String, n: Console): Console = Mu(PutLine(s, n))

  def stop: Console = Mu[ConsoleInstr](Stop)

  def execute(instr: Console): Unit = {
    val go = instr.fold[() => Unit] {
      case Stop          => () => ()
      case GetLine(k)    => () => k(readLine())()
      case PutLine(s, n) => () => {
        println(s)
        n()
      }
    }

    go()
  }
}
