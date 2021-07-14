package problems.medium

object MaximalSquare {
  def main(args: Array[String]): Unit = {
    val matrix: Array[Array[Char]] = Array(
      Array('1', '0', '1', '0', '0'),
      Array('1', '0', '1', '1', '1'),
      Array('1', '1', '1', '1', '1'),
      Array('1', '0', '0', '1', '0')
    )
    val res: Int = maximalSquare(matrix)
    println(s"max=$res, matrix:\n${matrix.map(_.mkString("[", ", ", "]")).mkString("\n")}")
  }

  def maximalSquare(matrix: Array[Array[Char]]): Int = {
    val m: Int = matrix.length
    val n: Int = matrix.head.length
    val dp: Array[Array[Int]] = Array.fill(m)(new Array[Int](n))
    var tmp = -1

    (0 until m).foreach { i =>
      dp(i)(0) = if (matrix.map(_.head).take(i + 1).contains('1')) 1 else 0
    }

    (0 until n).foreach { j =>
      dp(0)(j) = if (matrix.head.take(j + 1).contains('1')) 1 else 0
    }

    /*
    dp.zipWithIndex.foreach { case (arr, i) =>
      println(s"dp($i)=${arr.toList}")
    }
    */

    (1 until m).foreach { i =>
      (1 until n).foreach { j =>
        tmp = squares(i, j)
        dp(i)(j) = Math.max(Math.max(dp(i - 1)(j), dp(i)(j - 1)), tmp)
        // println(s"i=$i, j=$j, squares($i,$j)=$tmp, dp($i)($j)=${dp(i)(j)}")
      }
    }

    def squares(i: Int, j: Int): Int = {
      var max = -1
      var tmp = -1
      (0 to Math.min(i, j)).foreach { k =>
        tmp = square(i - k, j - k, i, j)
        if (max < tmp) max = tmp
        // println(s"k=$k, square(${i - k},${j - k},$i,$j)=${tmp}, max=${max}")
      }
      max
    }

    def square(m1: Int, n1: Int, m2: Int, n2: Int): Int = {
      (m1 to m2).foreach { i =>
        (n1 to n2).foreach { j =>
          if (matrix(i)(j) == '0') return 0
        }
      }
      (m2 - m1 + 1) * (n2 - n1 + 1)
    }

    dp(m - 1)(n - 1)
  }
}
