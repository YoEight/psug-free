package free

import List._

object ListExample {
  def main(args: Array[String]) {
    val xs = cons(1, cons(2, cons(3, cons(4, cons(5, nil)))))

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
  }
}
