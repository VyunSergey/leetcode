package problems.easy

object BackspaceStringCompare {
  def main(args: Array[String]): Unit = {
    val Array(s: String, t: String) = Console.in.readLine().split(" ")
    assert(s.forall(c => c.isLetter || c == '#') && t.forall(c => c.isLetter || c == '#'))
    val res: Boolean = backspaceCompare(s, t)
    println(res)
  }

  def backspaceCompare(s: String, t: String): Boolean = {
    var i = s.length - 1
    var j = t.length - 1
    var del = 0

    while(0 <= i || 0 <= j) {
      // println(s"i=$i j=$j s($i)=${if (i >= 0) s(i) else -1} t($j)=${if (j >= 0) t(j) else -1}")
      while(0 <= i && (del > 0 || s(i) == '#')) {
        // println(s"i=$i del=$del s($i)=${if (i >= 0) s(i) else -1}")
        if (s(i) == '#') del += 1 else del -= 1
        i -= 1
      }

      del = 0
      while(0 <= j && (del > 0 || t(j) == '#')) {
        // println(s"j=$j del=$del t($j)=${if (j >= 0) t(j) else -1}")
        if (t(j) == '#') del += 1 else del -= 1
        j -= 1
      }

      if ((if (0 <= i) s(i) else '@') != (if (0 <= j) t(j) else '@')) return false
      i -= 1
      j -= 1
    }
    // println(s"i=$i j=$j")

    i < 0 && j < 0
  }
}
