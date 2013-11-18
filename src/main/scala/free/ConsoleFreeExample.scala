package free

import free.FreeConsole._

object ConsoleFreeExample {
  def main(args: Array[String]) {
    val instrs = for {
      line <- getLine
      _    <- putLine("we got: " + line)
    } yield ()

    interpret(instrs)
  }
}
