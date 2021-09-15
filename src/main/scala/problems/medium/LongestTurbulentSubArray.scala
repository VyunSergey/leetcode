package problems.medium

object LongestTurbulentSubArray {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Console.in.readLine().split(",")
      .map(_.toIntOption).collect { case Some(x) => x }
    val res = maxTurbulenceSize(arr)
    println(res)
  }

  def maxTurbulenceSize(arr: Array[Int]): Int = {
    var maxLen = 0
    var currLen1 = 1
    var currLen2 = 1
    var prev1 = -2
    var prev2 = -2

    (0 until arr.length - 1).foreach { i =>
      // even > || odd <
      if (
        (i % 2 == 0 && arr(i) > arr(i + 1)) ||
          (i % 2 == 1 && arr(i) < arr(i + 1))
      ) {
        if (prev1 + 1 == i) currLen1 += 1
        else currLen1 = 1
        prev1 = i
        maxLen = Math.max(maxLen, currLen1)
      // even < || odd >
      } else if (
        (i % 2 == 0 && arr(i) < arr(i + 1)) ||
          (i % 2 == 1 && arr(i) > arr(i + 1))
      ) {
        if (prev2 + 1 == i) currLen2 += 1
        else currLen2 = 1
        prev2 = i
        maxLen = Math.max(maxLen, currLen2)
      }
    }
    maxLen + 1
  }
}
