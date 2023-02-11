package problems.easy

import scala.annotation.tailrec

object AddToArrayFormOfInteger {

  def main(args: Array[String]): Unit = {
    val num = Console.in.readLine().split(" ").map(_.toIntOption).collect { case Some(i) => i }
    val k = Console.in.readLine().toInt
    val res = addToArrayForm(num, k)
    println(s"The sum of ${num.mkString("[", ", ", "]")} and $k is ${res.mkString("[", ", ", "]")}")
  }

  @tailrec
  def add(k: Int, ind: Int, n: Int, num: Array[Int], res: List[Int] = Nil): List[Int] = {
    if (k == 0 && ind >= n) res
    else {
      val d = (if (ind < n) num(n - ind - 1) else 0) + (k % 10)
      add(k / 10 + d / 10, ind + 1, n, num, (d % 10) :: res)
    }
  }

  def addToArrayForm(num: Array[Int], k: Int): List[Int] = {
    add(k, 0, num.length, num)
  }
}
