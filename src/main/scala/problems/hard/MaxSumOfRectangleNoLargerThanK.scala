package problems.hard

object MaxSumOfRectangleNoLargerThanK {
  def main(args: Array[String]): Unit = {
    val k =Console.in.readLine().toInt
    val matrix: Array[Array[Int]] = Console.in.readLine().split("\\s+").map(_.split(",").map(_.toInt))
    assert(matrix.length >= 1)
    assert(matrix.forall(row => row.length == matrix.head.length))

    val res = maxSumSubmatrix(matrix, k)
    println(s"res=$res k=$k")
    matrix.foreach(row => println(row.map(x => f"$x%3d").mkString("[", "][", "]")))
  }

  def maxSumSubmatrix(matrix: Array[Array[Int]], k: Int): Int = {
    // a[0][0]   ... a[0][j-1]   a[0][j]   ... a[0][m]
    // ...       ... ...         ...       ... ...
    // a[i-1][0] ... a[i-1][j-1] a[i-1][j] ... a[i-1][m]
    // a[i][0]   ... a[i][j-1]   a[i][j]   ... a[i][m]
    // ...       ... ...         ...       ... ...
    // a[n][0]   ... a[n][j-1]   a[n][j]   ... a[n][m]

    val sum = k
    val n = matrix.length
    val m = matrix.head.length
    val dp = Array.fill(n+1, m+1)(0)
    val r = Array.fill(n+1, m+1)(0)
    val c = Array.fill(n+1, m+1)(0)
    var max = Int.MinValue

    for {
      i <- 1 to n
      j <- 1 to m
    } {
      r(i)(j) = r(i)(j-1) + matrix(i-1)(j-1)
      c(i)(j) = c(i-1)(j) + matrix(i-1)(j-1)
      dp(i)(j) = dp(i-1)(j-1) + r(i)(j-1) + c(i-1)(j) + matrix(i-1)(j-1)
      if (dp(i)(j) <= sum) {
        max = Math.max(max, dp(i)(j))
        if (max == sum) return max
      }
    }

/*
    // print
    for {
      i <- 0 to n
      _ = print("\n")
      j <- 0 to m
    } {
      print(f"[${dp(i)(j)}%3d]")
    }
*/

    for {
      i <- 0 to n
      j <- 0 to m
      k <- i + 1 to n
      l <- j + 1 to m
    } {
      val a = dp(k)(l) + dp(i)(j) - dp(k)(j) - dp(i)(l)
      if (a <= sum) {
        max = Math.max(max, a)
        if (max == sum) return max
      }
    }

    max
  }
}
