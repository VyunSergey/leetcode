package problems.medium

import scala.collection.mutable

object KLargestElementOfArray {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ").map(_.toInt)
    val k: Int = Console.in.readLine().toInt
    val res = findKthLargest(nums, k)
    println(res)
  }

  def findKthLargest(nums: Array[Int], k: Int): Int = {
    // println(nums.sorted.toList)
    nums.sorted.apply(nums.length - k)
  }

  def findKthLargestTwo(nums: Array[Int], k: Int): Int = {
    val queue = mutable.PriorityQueue.empty[Int]
    var res = -1
    queue ++= nums
    (0 until k).foreach { _ =>
      res = queue.dequeue
    }
    res
  }
}
