package problems.easy

import scala.annotation.tailrec

object SearchInsertPosition {
  def main(args: Array[String]): Unit = {
    val nums = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(x) => x }
    val target = Console.in.readLine().toInt
    assert(nums.nonEmpty)
    assert(nums.toSet.toArray.sorted.sameElements(nums))
    val res = searchInsert(nums, target)
    println(res)
  }

  def searchInsert(nums: Array[Int], target: Int): Int = {
    search(nums)(0, nums.length - 1, target)
  }

  @tailrec
  def search(nums: Array[Int])(left: Int, right: Int, target: Int): Int = {
    if (target < nums(left)) left
    else if (target > nums(right)) right + 1
    else {
      val mid = (left + right) >> 1

      if (target < nums(mid)) search(nums)(left, mid - 1, target)
      else if (target > nums(mid)) search(nums)(mid + 1, right, target)
      else mid
    }
  }
}
