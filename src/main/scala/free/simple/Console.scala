package free.simple

object Console {
  sealed trait Console
  case class ReadLine(f: String => Console) extends Console
  case class PutLine(s: String, next: Console) extends Console
  case object Stop extends Console

  @annotation.tailrec
  def execute(instr: Console): Unit = instr match {
    case Stop             => ()
    case ReadLine(k)      => execute(k(readLine()))
    case PutLine(s, next) =>
      println(s)
      execute(next)
  }
}
