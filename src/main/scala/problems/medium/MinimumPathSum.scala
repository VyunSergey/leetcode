package problems.medium

object MinimumPathSum {
  def main(args: Array[String]): Unit = {
    val arr: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) => x })
    assert(arr.nonEmpty && arr.forall(_.nonEmpty))
    assert(arr.forall(_.length == arr.head.length))
    val res: Int = minPathSum(arr)
    println(res)
  }

  def minPathSum(grid: Array[Array[Int]]): Int = {
    val m = grid.length
    val n = grid.head.length
    val dp = Array.fill(m, n)(0)

    dp(0)(0) = grid(0)(0)
    for(i <- 1 until m) dp(i)(0) = dp(i - 1)(0) + grid(i)(0)
    for(j <- 1 until n) dp(0)(j) = dp(0)(j - 1) + grid(0)(j)

    for {
      i <- 1 until m
      j <- 1 until n
    } {
      // println(s"i=$i j=$j")
      // println(s"dp(${i - 1})($j)=${dp(i - 1)(j)} dp($i)(${j - 1})=${dp(i)(j - 1)}")
      dp(i)(j) = Math.min(dp(i - 1)(j), dp(i)(j - 1)) + grid(i)(j)
    }

    dp(m - 1)(n - 1)
  }
}
