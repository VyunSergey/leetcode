package problems.medium

import scala.collection.mutable

object MaximumErasureValue {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(",").map(_.toIntOption).collect { case Some(x) => x }
    val res = maximumUniqueSubArray(nums)
    println(res)
  }

  def maximumUniqueSubArray(nums: Array[Int]): Int = {
    var currSum = 0
    var maxSum = Int.MinValue
    val set = mutable.HashSet.empty[Int]
    var i = 0
    var j = 0

    while(j < nums.length) {
      while(set.contains(nums(j))) {
        // Removing the ith element until we reach the repeating element
        set -= nums(i)
        currSum -= nums(i)
        i += 1
      }
      // Add the current element to set and currSum value
      currSum += nums(j)
      set += nums(j)
      j += 1

      // maxSum variable to keep track of largest currSum encountered till now
      maxSum = Math.max(maxSum, currSum)
    }

    maxSum
  }
}
