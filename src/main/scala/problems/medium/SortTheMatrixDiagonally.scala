package problems.medium

object SortTheMatrixDiagonally {
  def main(args: Array[String]): Unit = {
    val mat: Array[Array[Int]] = Console.in.readLine().split("\\s+").map(_.split(",").map(_.toInt))
    mat.foreach(line => println(line.mkString("[", "][", "]")))
    println()
    val res = diagonalSort(mat)
    res.foreach(line => println(line.mkString("[", "][", "]")))
  }

  def diagonalSort(mat: Array[Array[Int]]): Array[Array[Int]] = {
    val n = mat.length
    val m = mat.head.length

    for(k <- 0 until n + m - 1) {
      // mat(i)(j) -> mat(i+s)(j+s)
      val i = if (k < n) (n - 1) - k else 0
      val j = if (k < n) 0 else k - (n - 1)
      val l = Math.min(n - i, m - j)
      val arr = Array.fill(l)(0)
      // println(s"i=$i j=$j l=$l")
      for(s <- 0 until l) {
        arr(s) = mat(i+s)(j+s)
      }
      val sort = arr.sorted
      for(s <- 0 until l) {
        mat(i+s)(j+s) = sort(s)
      }
    }

    mat
  }
}
