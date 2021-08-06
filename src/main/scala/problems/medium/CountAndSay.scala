package problems.medium

import scala.collection.mutable

object CountAndSay {
  def main(args: Array[String]): Unit = {
    val n: Int = Console.in.readLine().toInt
    assert(n >= 1)
    val res: String = countAndSay(n)
    println(res.length)
    println(res)
  }

  def countAndSay(n: Int): String = {
    if (n == 1) "1"
    else parse(countAndSay(n - 1))
  }

  def parse(str: String): String = {
    if (str.isEmpty) return str
    var i = 1
    var chr = str.head
    var count = 1
    val buf = new mutable.StringBuilder()

    while(i < str.length) {
      if (str(i) == chr) count += 1
      else {
        buf ++= s"$count$chr"
        chr = str(i)
        count = 1
      }
      i += 1
    }
    if (count > 0) {
      buf ++= s"$count$chr"
    }
    buf.result
  }
}
