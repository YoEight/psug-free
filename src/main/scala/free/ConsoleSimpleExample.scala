package free

import simple.Console._

object ConsoleSimple {
  def main(args: Array[String]) {
    val instrs =
      GetLine {
        line =>
          PutLine("we got: " ++ line, Stop)
      }

    execute(instrs)
  }
}
