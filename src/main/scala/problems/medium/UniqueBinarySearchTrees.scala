package problems.medium

object UniqueBinarySearchTrees {
  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    val res: Int = numTrees(n)
    println(res)
  }

  // cached results for small n nodes unique Binary Search Trees
  val cache = Map(
    0 -> 1,
    1 -> 1,
    2 -> 2,
    3 -> 5,
    4 -> 14,
    5 -> 42,
    6 -> 132,
    7 -> 429
  )

  def dpTrees(n: Int): Array[Int] = {
    val dp = new Array[Int](n + 1)

    cache.foreach { case (i, v) => dp(i) = v }

    dp.indices
      .filterNot(cache.contains)
      .foreach { i =>
        (0 to (i - 1) / 2).foreach { j =>
          if (j < (i - 1).toDouble / 2.0) {
            dp(i) += 2 * dp(j) * dp(i - 1 - j)
          } else {
            dp(i) += dp(j) * dp(j)
          }
        }
      }
    dp
  }

  // the number of structurally unique Binary Search Trees which has exactly n nodes
  def numTrees(n: Int): Int = {
    // f(n) = f(n - 1 - k) * f(k)
    // k = 0, 1, ..., n - 1
    // f(0) = 1
    // f(1) = 1
    // f(2) = 2
    // f(3) += f(0)*f(2)
    // f(3) += f(2)*f(0)
    // f(3) += f(1)*f(1)
    cache.getOrElse(n,
      {
        val dp = dpTrees(n)
        // println(dp.toList)
        dp(n)
      }
    )
  }
}
