package problems.hard

object BurstBalloons {
  def main(args: Array[String]): Unit = {
    val balloons = Console.in.readLine().split(",")
      .map(_.toIntOption).collect { case Some(x) => x }
    val res = maxCoins(balloons)
    println(res)
  }

  def maxCoins(nums: Array[Int]): Int = {
    // add one before and one after nums
    val n: Int = nums.length + 2
    val newNums: Array[Int] = new Array[Int](n)
    // copy nums into newNums
    nums.indices.foreach(i => newNums(i + 1) = nums(i))
    newNums(0) = 1
    newNums(n - 1) = 1

    // dp(left)(right) represents maximum of coins
    // if we burst all nums(left)...nums(right), inclusive
    val dp: Array[Array[Int]] = Array.fill(n)(new Array[Int](n))
    // do not include the first one and the last one
    // since they are both fake balloons added by ourselves and we can not burst them
    Range(n - 2, 0, -1).foreach { left =>
      Range(left, n - 1, 1).foreach { right =>
        // find the last burst one in newNums(left)...newNums(right)
        Range(left, right + 1, 1).foreach { i =>
          // newNums(i) is the last burst one
          val gain: Int = newNums(left - 1) * newNums(i) * newNums(right + 1)
          // recursively call left and right sides
          val remaining: Int = dp(left)(i - 1) + dp(i + 1)(right)
          // update
          dp(left)(right) = Math.max(remaining + gain, dp(left)(right))
        }
      }
    }
    // burst newNums(1)...newNums(n-2), excluding the first one and the last one
    dp(1)(n - 2)
  }
}
