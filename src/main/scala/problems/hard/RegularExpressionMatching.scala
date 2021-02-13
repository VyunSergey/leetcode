package problems.hard

object RegularExpressionMatching {
  def main(args: Array[String]): Unit = {
    val s = Console.in.readLine
    val p = Console.in.readLine
    Console.out.println(isMatch(s, p))
  }

  def isMatch(s: String, p: String): Boolean = {
    s.matches(p)
  }
}
