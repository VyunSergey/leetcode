package study.array

object MaximumProductSubArray {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(i) => i }
    println(nums.toList)

    val res: Int = maxProduct(nums)
    println(res)
  }

  def maxProductNonZero(nums: Array[Int]): Int = {
    if (nums.isEmpty) return Int.MinValue
    val n = nums.length
    val dp = Array.fill(n)(0)
    var max = nums(0)

    dp(0) = nums(0)
    (1 until n).foreach { i =>
      dp(i) = dp(i - 1) * nums(i)
      max = Math.max(max, dp(i))
    }

    (0 until n).foreach { i =>
      (i + 1 until n).foreach { j =>
        max = Math.max(max, dp(j) / dp(i))
      }
    }
    max
  }

  def maxProduct(nums: Array[Int]): Int = {
    val n = nums.length
    var start = 0
    var end = 0
    var max = Int.MinValue

    while(end < n) {
      if (nums(end) != 0) end += 1
      else {
        max = Math.max(max, maxProductNonZero(nums.slice(start, end)))
        start = end + 1
        end += 1
      }
      // println((start, end, max))
    }
    max = Math.max(max, maxProductNonZero(nums.slice(start, end)))
    if (start > 0) max = Math.max(max, 0)
    max
  }
}
