package problems.medium

object DecodeWays {
  def main(args: Array[String]): Unit = {
    val string = Console.in.readLine()
    assert(string.nonEmpty)
    assert(string.forall(('0' to '9').contains))
    val res = numDecoding(string)
    println(res)
  }

  def numDecoding(s: String): Int = {
    if (s == null || s.isEmpty) return 0

    val n = s.length
    val dp = new Array[Int](n + 1)

    dp(0) = 1
    dp(1) = if (s(0) != '0') 1 else 0

    (2 to n).foreach { i =>
      val first = s.slice(i - 1, i).toInt
      val second = s.slice(i - 2, i).toInt
      if (first >= 1 && first <= 9) {
        dp(i) += dp(i - 1)
      }
      if (second >= 10 && second <= 26) {
        dp(i) += dp(i - 2)
      }
    }

    dp(n)
  }

}
