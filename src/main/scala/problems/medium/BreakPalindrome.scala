package problems.medium

object BreakPalindrome {
  def main(args: Array[String]): Unit = {
    val palindrome = Console.in.readLine()
    assert(palindrome.nonEmpty)
    assert(palindrome.forall(chr => chr.isLetter && chr.isLower))
    assert {
      val len = palindrome.length
      val (left , right) = palindrome.splitAt((len + 1) / 2)
      left.take(len / 2) == right.reverse.take(len / 2)
    }
    val res = breakPalindrome(palindrome)
    println(res)
  }

  def breakPalindrome(palindrome: String): String = {
    val len = palindrome.length
    if (len <= 1) return ""

    (0 until len / 2).foreach { i =>
      if (palindrome(i) != 'a') return palindrome.updated(i, 'a')
    }

    palindrome.updated(len - 1, 'b')
  }
}
