package problems.easy

import scala.annotation.tailrec

object TwoSumTwo {
  def main(args: Array[String]): Unit = {
    val numbers = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(x) => x }
    val target = Console.in.readLine().toInt
    assert(numbers.nonEmpty)
    assert(numbers.toList.sorted == numbers.toList)
    val Array(i1, i2) = twoSum(numbers, target)
    println(s"(i1=${i1 - 1} i2=${i2 - 1}) " +
      s"numbers(${i1 - 1})+numbers(${i2 - 1})=${numbers(i1 - 1)}+${numbers(i2 - 1)}=$target")
  }

  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    numbers.indices.foreach { ind =>
      val indOp = binSearch(numbers, 0, numbers.length - 1, target - numbers(ind))
      indOp.map { ind2 =>
        if (ind != ind2) return Array(Math.min(ind, ind2) + 1, Math.max(ind, ind2) + 1)
      }
    }
    Array.empty[Int]
  }

  @tailrec
  def binSearch(nums: Array[Int], left: Int, right: Int, target: Int): Option[Int] = {
    val mid = (left + right) / 2
    val num = nums(mid)
    // println(s"(left=$left mid=$mid right=$right) num=$num target=$target")

    if (num == target) Some(mid)
    else if (left > right) None
    else if (num < target) binSearch(nums, mid + 1, right, target)
    else binSearch(nums, left, mid - 1, target)
  }
}
