package problems.medium

object RotateFunction {
  def main(args: Array[String]): Unit = {
    val nums = Console.in.readLine().split("\\s+").map(_.toInt)
    val res = maxRotateFunction(nums)
    println(s"res=$res nums=${nums.mkString(",")}")
  }

  // Max F(k) = 0 * arr[k] + 1 * arr[k+1] + ... + (n - 1) * arr[k + n - 1]
  def maxRotateFunction(nums: Array[Int]): Int = {
    // F(k) = 0 * arr[k] + 1 * arr[k+1] + ... + (n - 1) * arr[k + n - 1]
    // F(k+1) = 0 * arr[k+1] + 1 * arr[k+2] + ... + (n - 2) * arr[k + n - 1] + (n - 1) * arr[k + n]
    // F(k+1) = F(k) - (arr[k] + arr[k+1] + ... + arr[k + n - 1]) + arr[k] + (n - 1) * arr[k + n]
    // F(k+1) = F(k) - S + arr[k] + (n - 1) * arr[k + n], S = arr[0] + ... arr[n - 1]
    // F(k+1) = F(k) - S + n * arr[k]

    val n = nums.length
    val dp = Array.fill(n)(0)
    var sum = 0
    var f0 = 0

    for(i <- 0 until n) {
      sum += nums(i)
      f0 += i * nums(i)
    }

    dp(0) = f0
    for(k <- 0 until n - 1) {
      dp(k + 1) = dp(k) - sum + n * nums(k)
    }

    // dp.foreach(println)

    dp.max
  }
}
