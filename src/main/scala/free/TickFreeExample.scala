package free

import free.Tick._

object TickFreeExample {
  def main(args: Array[String]){
    val instr = for {
      i <- getTick
      _ <- modifiyTick(_ + 1)
    } yield i

    print(interpret(instr))
  }
}
