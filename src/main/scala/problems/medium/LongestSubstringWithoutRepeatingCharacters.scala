package problems.medium

object LongestSubstringWithoutRepeatingCharacters {
  def main(args: Array[String]): Unit = {
    val s = Console.in.readLine
    Console.out.println(lengthOfLongestSubstring(s))
  }

  def lengthOfLongestSubstring(s: String): Int = {
    val len = s.length
    val lenM = s.toSet.size
    var max = Math.min(len, 1)

    for {
      k <- Range(lenM, 1, -1) if k > max
      i <- Range(0, len - k + 1, 1) if k > max
    } yield {
      val ss = s.substring(i, i + k)
      max = Math.max(
        max,
        if (ss.toSet.size == ss.length) k else 0)
    }

    max
  }
}
