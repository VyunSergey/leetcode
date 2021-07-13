package problems.medium

import scala.annotation.tailrec

object FindPeakElement {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Console.in.readLine().split(" ").map(_.toInt)
    val res: Int = findPeakElement(arr)
    println(s"ind=$res, elem=${arr(res)}")
  }

  def findPeakElement(nums: Array[Int]): Int = {
    @tailrec
    def pick(left: Int, right: Int): Int = {
      if (left == right) return left
      val mid = (left + right) / 2
      // println((left, mid, mid + 1, right))
      if (nums(mid) < nums(mid + 1)) pick(mid + 1, right)
      else pick(left, mid)
    }

    pick(0, nums.length - 1)
  }
}
