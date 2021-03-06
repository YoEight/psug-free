package free

import mu.ListInstr._

object ListMuExample {
  def main(args: Array[String]) {
    val xs = cons(1, cons(2, cons(3, cons(4, cons(5, nil)))))
    val vs = cons(6, cons(7, cons(8, cons(9, cons(10, nil)))))

    print(show(xs))
    print(" -- Start list")
    println()

    print(show(map(xs)(_ + 2)))
    print(" -- Add 2 to every element")
    println()

    print(show(filter(xs)(_ % 2 == 0)))
    print(" -- Filter even number")
    println()

    print(find(xs)(_ == 3))
    print(" -- Find number 3")
    println()

    print(foldRight(xs, 0)(_ + _))
    print(" -- Sum every element")
    println()

    foldRight(xs, ()){ case (a, _) => print("(" + a + ")")}
    print(" -- Right traversal")
    println()

    foldLeft(xs, ()){ case (_, a) => print("(" + a + ")")}
    print(" -- Left traversal")
    println()

    print(show(append(xs, vs)))
    print(" -- Append")
    println()
  }
}
