package problems.medium

object PalindromicSubstrings {
  def main(args: Array[String]): Unit = {
    val str: String = Console.in.readLine().filter(_.isLetter)
    val res: Int = countSubstrings(str)
    println(res)
  }

  def countSubstrings(s: String): Int = {
    val chars = s.toCharArray
    val len = chars.length
    var i = 0
    var j = 0
    var cnt = 0
    while (i < len) {
      j = i
      while (j < len) {
        val sub = chars.slice(i, j + 1)
        // println((i, j, sub.toList, sub.reverse.toList))
        if (sub.sameElements(sub.reverse)) cnt += 1
        j += 1
      }
      i += 1
    }
    cnt
  }

}
