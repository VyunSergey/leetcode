package problems.hard

import scala.collection.mutable

object MinimumWindowSubstring {
  def main(args: Array[String]): Unit = {
    val s = Console.in.readLine
    val t = Console.in.readLine
    val res = minWindow(s, t)
    println(s"'$res'")
  }

  def minWindow(s: String, t: String): String = {
    var i = 0
    val record = new Array[Int](Math.max('z', 'Z') + 1)
    val appear = mutable.Map.empty[Char, Boolean]

    t.foreach { char =>
      record(char) = record(char) + 1
      appear += (char -> true)
    }

    //println(record.mkString("[", ",", "]"))
    //println(appear)

    var bestLen = Int.MaxValue
    var bestStart = 0
    var bestEnd = s.length
    var missing = t.length
    var end = 0
    var sChar = '0'
    var endChar = '0'
    i = 0

    while (i < s.length) {
      sChar = s(i)
      if (appear.contains(sChar)) {
        while (missing != 0 && end < s.length) {
          endChar = s(end)
          if (appear.contains(endChar)) {
            if (record(endChar) > 0) missing -= 1
            record(endChar) = record(endChar) - 1
            //println(record.mkString("[", ",", "]"))
          }
          end += 1
        }
        if (bestLen > (end - i) && missing == 0) {
          bestLen = end - i
          bestEnd = end
          bestStart = i
          //println((s.substring(bestStart, bestEnd), bestLen, bestStart, bestEnd))
        }
        if (record(sChar) >= 0) missing += 1
        record(sChar) = record(sChar) + 1
        //println(record.mkString("[", ",", "]"))
      }
      i += 1
    }

    if (bestLen > s.length) "" else s.substring(bestStart, bestEnd)
  }
}
