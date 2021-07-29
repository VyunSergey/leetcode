package problems.medium

object ZeroOneMatrix {
  def main(args: Array[String]): Unit = {
    val matrix: Array[Array[Int]] =
      Console.in.readLine().split(" ").map(_.split(",").map(_.toInt))
    val res: Array[Array[Int]] = updateMatrix(matrix)
    println(res.map(_.mkString("[", ", ", "]")).mkString("\n"))
  }

  def updateMatrix(mat: Array[Array[Int]]): Array[Array[Int]] = {
    val rows = mat.length
    if (rows == 0) return mat

    val cols = mat.head.length
    val dist = Array.fill(rows)(Array.fill(cols)(Int.MaxValue - 100000))

    // First pass: check for left and top
    Range(0, rows, 1).foreach {i =>
      Range(0, cols, 1).foreach { j =>
        // println(s"first i=$i j=$j")
        if (mat(i)(j) == 0) {
          dist(i)(j) = 0
        } else {
          if (i > 0) dist(i)(j) = Math.min(dist(i)(j), dist(i - 1)(j) + 1)
          if (j > 0) dist(i)(j) = Math.min(dist(i)(j), dist(i)(j - 1) + 1)
        }
      }
    }

    // Second pass: check for bottom and right
    Range(rows - 1, -1, -1).foreach { i =>
      Range(cols - 1, -1, -1).foreach { j =>
        // println(s"second i=$i j=$j")
        if (i < rows - 1) dist(i)(j) = Math.min(dist(i)(j), dist(i + 1)(j) + 1)
        if (j < cols - 1) dist(i)(j) = Math.min(dist(i)(j), dist(i)(j + 1) + 1)
      }
    }

    dist
  }
}
