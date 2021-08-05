package problems.medium

object StoneGame {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(n) => n }
    assert(arr.length > 1 && arr.length % 2 == 0 && arr.sum % 2 == 1)
    val res: Boolean = stoneGame(arr)
    println(res)
  }

  def stoneGame(piles: Array[Int]): Boolean = {
    val len = piles.length
    if (len == 2) return true

    // dp(i)(j) = (first-max, second-max) solution for [i, j] slice of piles
    val dp: Array[Array[(Int, Int)]] = Array.fill(len)(Array.fill(len)((0, 0)))

    // dp(i)(i + 1) = (max(p(i), p(i + 1)), min(p(i), p(i + 1)))
    (0 until len - 2).foreach { i =>
      dp(i)(i + 1) = (Math.max(piles(i + 1), piles(i)),
        Math.min(piles(i + 1), piles(i)))
    }

    // dp(i)(i + j)
    (2 until len).foreach { j =>
      (0 until len - j).foreach { i =>
        val a = piles(i) + dp(i + 1)(i + j)._2
        val b = piles(i + j) + dp(i)(i + j - 1)._2
        if (a > b) {
          val c = piles(i + 1) + dp(i + 2)(i + j)._2
          val d = piles(i + j) + dp(i + 1)(i + j - 1)._2
          dp(i)(i + j) = (a, Math.max(c, d))
        } else {
          val c = piles(i) + dp(i + 1)(i + j - 1)._2
          val d = piles(i + j - 1) + dp(i)(i + j - 2)._2
          dp(i)(i + j) = (b, Math.max(c, d))
        }
        // println(s"find dp($i)(${i + j}) = ${dp(i)(i + j)}")
      }
    }

    val (first, second) = dp(0)(len - 1)
    first > second
  }
}
