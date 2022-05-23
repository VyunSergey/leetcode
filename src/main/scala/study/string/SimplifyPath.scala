package study.string

import scala.collection.mutable

object SimplifyPath {
  def main(args: Array[String]): Unit = {
    val path = Console.in.readLine().filter(c => List('.', '/', '_').contains(c) || c.isDigit || c.isLetter)

    val res = simplifyPath(path)
    println(s"res=$res")
  }

  def simplifyPath(path: String): String = {
    // ./ad/.././sd/./../sd/
    // [ad] [..] [sd] [..] [sd]
    // (0, [ad])
    // (1, [..])
    val simplified: Array[String] = path.split('/').filterNot(c => c.isEmpty || c == ".")

    var i = 0
    val buf = mutable.ArrayBuffer.empty[(Int, String)]

    simplified.foreach { str =>
      println(str)
      if (str == "..") {
        buf.filterInPlace(_._1 != i - 1)
        i -= 1
      } else {
        buf += ((i, str))
        i += 1
      }
    }

    buf.map(_._2).mkString("/", "/", "")
  }
}
