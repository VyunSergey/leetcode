package study.string

object AddBinary {
  def main(args: Array[String]): Unit = {
    val Array(a, b) = Console.in.readLine().split(" ")
      .map(_.filter(List('0', '1').contains).mkString).take(2)
    println(s"a=$a", s"b=$b")

    val res = addBinary(a, b)
    println(res)
  }

  def addBinary(a: String, b: String): String = {
    val n = a.length
    val m = b.length
    var inMind = 0
    var res = ""

    (0 until Math.max(n, m)).foreach { i =>
      val d1: Int = if (i < n) a(n - 1 - i) - '0' else 0
      val d2: Int = if (i < m) b(m - 1 - i) - '0' else 0
      res = ((d1 + d2 + inMind) % 2).toString + res
      inMind = (d1 + d2 + inMind) / 2
      // println((d1, d2, inMind, res))
    }
    if (inMind > 0) inMind.toString + res else res
  }
}
