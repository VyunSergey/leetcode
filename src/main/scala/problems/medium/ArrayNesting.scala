package problems.medium

import scala.collection.mutable

object ArrayNesting {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(",")
      .map(_.toIntOption).collect { case Some(x) => x }
    val res = arrayNesting(nums)
    println(res)
  }

  def arrayNesting(nums: Array[Int]): Int = {
    if (nums.length <= 1) return 1

    var num = 0
    var size = 0
    var maxSize = 1
    val visited = mutable.HashSet.empty[Int]

    nums.foreach { start =>
      if (!visited.contains(start)) {
        size = 0
        num = start
        while(!visited.contains(num)) {
          size += 1
          visited += num
          num = nums(num)
        }
        maxSize = Math.max(maxSize, size)
      }
    }

    maxSize
  }
}
