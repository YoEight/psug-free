package free

import simple.Console._

object ConsoleSimple {
  def main(args: Array[String]) {
    val instrs =
      ReadLine {
        line =>
          PutLine("we got: " ++ line, Stop)
      }

    execute(instrs)
  }
}
