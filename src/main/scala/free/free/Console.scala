package free
package free

import mu.ConsoleInstr

object FreeConsole {
  def getLine: Free[ConsoleInstr, String] =
    Free.suspend[ConsoleInstr, String](mu.GetLine(Free.emit))

  def putLine(s: String): Free[ConsoleInstr, Unit] =
    Free.suspend[ConsoleInstr, Unit](mu.PutLine(s, Free.emit()))

  def interpret[A](instr: Free[ConsoleInstr, A]): A = {
    val go = instr.fold[ConsoleInstr, () => A](a => () => a) {
      case mu.Stop          => ???
      case mu.GetLine(k)    => () => k(readLine())()
      case mu.PutLine(s, n) => () => {
        println(s)
        n()
      }
    }

    go()
  }
}
