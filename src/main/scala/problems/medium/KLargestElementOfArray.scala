package problems.medium

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
}
