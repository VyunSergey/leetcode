package problems.hard

object MaximalRectangle {
  def main(args: Array[String]): Unit = {
    val matrix: Array[Array[Char]] = Array(
      Array('1', '0', '1', '0', '0', '1', '0'),
      Array('1', '0', '1', '1', '1', '0', '0'),
      Array('1', '1', '1', '1', '1', '1', '1'),
      Array('1', '0', '0', '1', '1', '1', '1'),
      Array('1', '0', '0', '1', '0', '0', '1')
    )
    val res: Int = maximalRectangle(matrix)
    println(s"max=$res, matrix:\n${matrix.map(_.mkString("[", ", ", "]")).mkString("\n")}")
  }

  def maximalRectangle(matrix: Array[Array[Char]]): Int = {
    val m: Int = matrix.length
    val n: Int = matrix.headOption.map(_.length).getOrElse(0)
    val dp: Array[Array[Int]] = Array.fill(m)(new Array[Int](n))

    if (m == 0 || n == 0) return 0

    def maxLineH(i: Int, init: Int): Int = {
      var k = 0
      var max = init
      while (k <= i && i + 1 - k > max) {
        if (max < i + 1 - k
          && matrix.head.slice(k, i + 1).forall(_ == '1')
        ) max = i + 1 - k
        k += 1
      }
      max
    }

    def maxLineV(i: Int, init: Int): Int = {
      var k = 0
      var max = init
      while (k <= i && i + 1 - k > max) {
        if (max < i + 1 - k
          && matrix.map(_.head).slice(k, i + 1).forall(_ == '1')
        ) max = i + 1 - k
        k += 1
      }
      max
    }

    /*
    matrix.zipWithIndex.foreach { case (arr, i) =>
      println(s"m($i)=${arr.toList}")
    }
    */

    dp(0)(0) = if (matrix(0)(0) == '1') 1 else 0

    (1 until m).foreach { i =>
      dp(i)(0) =
        if (matrix(i)(0) == '0') dp(i - 1)(0)
        else maxLineV(i, dp(i - 1)(0))
    }

    (1 until n).foreach { j =>
      dp(0)(j) =
        if (matrix(0)(j) == '0') dp(0)(j - 1)
        else maxLineH(j, dp(0)(j - 1))
    }

    /*
    dp.zipWithIndex.foreach { case (arr, i) =>
      println(s"dp($i)=${arr.toList}")
    }
    */

    (1 until m).foreach { i =>
      (1 until n).foreach { j =>
        dp(i)(j) =
          if (matrix(i)(j) == '0') Math.max(dp(i - 1)(j), dp(i)(j - 1))
          else rectangles(i, j, Math.max(dp(i - 1)(j), dp(i)(j - 1)))
        // println(s"\ni=$i, j=$j, dp($i)($j)=${dp(i)(j)}, init=${Math.max(dp(i - 1)(j), dp(i)(j - 1))}\n")
      }
    }

    def rectangles(i: Int, j: Int, init: Int): Int = {
      var k = i
      var l = j
      var max = init
      var tmp = 0

      while (k >= 0 && (k + 1).toDouble > max.toDouble / (j + 1).toDouble) {
        l = j
        while (l >= 0 && (l + 1).toDouble > max.toDouble / (k + 1).toDouble) {
          if (matrix(i - k)(j - l) == '1') tmp = rectangle(i - k, j - l, i, j)
          if (max < tmp) max = tmp
          // println(s"k=$k, l=$l, m(${i - k})(${j - l})=${matrix(i - k)(j - l)}, rec(${i - k},${j - l},$i,$j)=$tmp, max=$max, ${(k + 1).toDouble}>${max.toDouble / (l + 1).toDouble}, ${(l + 1).toDouble}>${max.toDouble / (k + 1).toDouble}")
          l -= 1
        }
        k -= 1
      }
      max
    }

    def rectangle(m1: Int, n1: Int, m2: Int, n2: Int): Int = {
      var i = m1
      var j = n1

      // println(s"rec($m1,$n1,$m2,$n2)")
      while (i <= m2) {
        j = n1
        while (j <= n2) {
          // println(s"i=$i, j=$j, m($i)($j)=${matrix(i)(j)}")
          if (matrix(i)(j) == '0') return 0
          j += 1
        }
        i += 1
      }
      (m2 - m1 + 1) * (n2 - n1 + 1)
    }

    dp(m - 1)(n - 1)
  }
}
