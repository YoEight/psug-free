package free

import mu.ConsoleInstr._

object ConsoleMuExample {
  def main(args: Array[String]) {
    val instrs =
      getLine {
        line =>
          putLine("we got: " + line, stop)
      }

    execute(instrs)
  }
}
