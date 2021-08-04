package problems.easy

import scala.annotation.tailrec

object GrayCode {
  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val res: List[Int] = grayCode(n)
    println(res)
  }
  // n = 1
  // 0 1
  // n = 2
  // 00 01      | 11 10
  // 00 + (0 1) | 10 + (1, 0)
  // n = 3
  // 000 001 011 010     | 110 111 101 100
  // 000 + (00 01 11 10) | 100 + (10 11 01 00)
  def grayCode(n: Int): List[Int] = {
    if (n > 2) code(n - 2, 2, List(0, 1, 3, 2))
    else if (n == 2) List(0, 1, 3, 2)
    else List(0, 1)
  }

  @tailrec
  def code(n: Int, k: Int, res: List[Int]): List[Int] = {
    if (n == 0) res
    else code(n - 1, k + 1,
      res ++ res.reverseIterator.map(x => (1 << k) + x))
  }
}
