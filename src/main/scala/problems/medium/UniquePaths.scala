package problems.medium

object UniquePaths {
  def main(args: Array[String]): Unit = {
    val Array(m, n) = Console.in.readLine().split(" ").map(_.toInt).take(2)
    val res = uniquePaths(m, n)
    println(res)
  }

  def uniquePaths(m: Int, n: Int): Int = {
    def printPaths(paths: Array[Array[Int]]): Unit = {
      paths.map(_.toList).foreach(println)
      println()
    }

    val paths: Array[Array[Int]] = Array.fill(m)(new Array[Int](n))
    printPaths(paths)

    (0 until m).foreach { i =>
      paths(i)(0) = 1
    }

    (0 until n).foreach { i =>
      paths(0)(i) = 1
    }
    printPaths(paths)

    for {
      i <- 1 until m
      j <- 1 until n
    } yield paths(i)(j) = paths(i - 1)(j) + paths(i)(j - 1)
    printPaths(paths)

    if (m > 0 && n > 0) paths(m - 1)(n - 1)
    else 0
  }
}
