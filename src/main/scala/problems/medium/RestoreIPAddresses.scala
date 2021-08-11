package problems.medium

import scala.collection.mutable

object RestoreIPAddresses {
  def main(args: Array[String]): Unit = {
    val str: String = Console.in.readLine()
    val res: List[String] = restoreIpAddresses(str)
    println(res.mkString("\n"))
  }

  def restoreIpAddresses(s: String): List[String] = {
    val len = s.length
    if (len < 4 || len > 12) return List.empty[String]

    val buf = mutable.ListBuffer.empty[String]

    Set.range(1, len).subsets(3).foreach { ids =>
      val List(a, b, c): List[Int] = ids.toList.sorted
      val s1 = s.slice(0, a)
      val s2 = s.slice(a, b)
      val s3 = s.slice(b, c)
      val s4 = s.slice(c, s.length)
      val ls = List(s1, s2, s3, s4)
      if (valid(ls)) buf += ls.mkString(".")
    }
    buf.result
  }

  def valid(ls: List[String]): Boolean = {
    ls.forall { str =>
      str.nonEmpty &&
        str.toIntOption.exists { i =>
          0 <= i && i <= 255 &&
            i.toString == str
        }
    }
  }

}
