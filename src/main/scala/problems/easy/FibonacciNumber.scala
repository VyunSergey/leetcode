package problems.easy

import scala.annotation.tailrec

object FibonacciNumber {
  def main(args: Array[String]): Unit = {
    val n = Console.in.readLine().toInt
    val res = (fib(n), fibRec(n), fibStream(n))
    println(res)
  }

  def fib(n: Int): Int = {
    var a = 0
    var b = 1
    var c = -1
    (0 until n).foreach { _ =>
      c = b
      b = a + b
      a = c
    }
    a
  }

  @tailrec
  def fibRec(n: Int, a: Int = 0, b: Int = 1): Int = {
    if (n == 0) a else fibRec(n - 1, b, a + b)
  }

  def fibStream(n: Int): Int = {
    lazy val fibs: LazyList[Int] =
      0 #:: 1 #:: fibs.zip(fibs.tail).map { case (a, b) => a + b }
    fibs(n)
  }
}
