package problems.easy

import scala.annotation.tailrec

object BinarySearch {
  def main(args: Array[String]): Unit = {
    val nums = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(x) => x }
    val target = Console.in.readLine().toInt
    val res = search(nums, target)
    println(res)
  }

  def search(nums: Array[Int], target: Int): Int = {
    binSearch(nums)(0, nums.length - 1, target)
  }

  @tailrec
  def binSearch(nums: Array[Int])(left: Int, right: Int, target: Int): Int = {
    val mid = (left + right) >> 1

    if (left > right) -1
    else if (nums(mid) == target) mid
    else if (nums(mid) < target) binSearch(nums)(mid + 1, right, target)
    else binSearch(nums)(left, mid - 1, target)
  }
}
