
object Main {
  def abs(n: Int): Int = if (n < 0) -n else n

  def factorial(n: Int): Int =
    Array.range(1, n+1).reduce(_ * _)

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(c: Int, p1: Int, p2: Int): Int =
      if (c > n)
        p1
      else
        go(c+1, p1+p2, p1)
    go(1, 0, 1)
  }

  private def formatResult(name: String, x: Int, f: Int => Int) =
    "The %s of %d is %s".format(name, x, f(x))

  def isSorted[A](as: Array[A], cmp: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, sorted: Boolean): Boolean =
      if (n >= as.length) sorted
      else if (!cmp(as(n-1), as(n))) false
      else loop(n+1, true)
    loop(1, true)
  }

  def partial[A,B,C](a: A, f: (A,B) => C): B => C =
    b => f(a, b)

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatResult("ABS", -42, abs))
    println(formatResult("factorial", 5, factorial))
    println(Array.range(0, 11).map(fib).mkString(","))
    println(isSorted(Array.range(5, 0, -1), (a:Int, b:Int) => a < b))
  }
}