package problems.medium

object LongestPalindromicSubstring {
  def main(args: Array[String]): Unit = {
    val s = Console.in.readLine
    Console.out.println(longestPalindrome(s))
  }

  def longestPalindrome(s: String): String = {
    val len = s.length
    var ss = s.take(1)
    var max = ss.length
    for {
      k <- Range(len + 1, 1, -1) if k > max
      i <- Range(0, len - k + 1, 1) if k > max
    } yield {
      val sb = s.substring(i, i + k)
      val sl = if (sb == sb.reverse) sb.length else 0
      ss = if (max > sl) ss else sb
      max = Math.max(max, sl)
    }
    ss
  }
}
