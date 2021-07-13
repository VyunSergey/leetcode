package problems.medium

import scala.collection.mutable

object PartitionEqualSubsetSum {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ").map(_.toInt)
    val res: Boolean = canPartition(nums)
    println(res)
  }

  def canPartition(nums: Array[Int]): Boolean = {
    var i = 0
    var sum = 0
    nums.foreach { num =>
      sum += num
    }

    if (sum % 2 == 1) return false
    sum /= 2

    val map = mutable.Map.empty[Int, Boolean]
    map += (0 -> true)

    nums.foreach { num =>
      i = sum
      while(i > 0) {
        if (i >= num) {
          map += (i -> (map.getOrElse(i, false) || map.getOrElse(i - num, false)))
        }
        i -= 1
      }
    }

    map.getOrElse(sum, false)
  }
}
