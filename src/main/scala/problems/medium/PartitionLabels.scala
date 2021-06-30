package problems.medium

import scala.collection.mutable

object PartitionLabels {
  def main(args: Array[String]): Unit = {
    val str = Console.in.readLine()
    val res = partitionLabels(str)
    println(res)
  }

  def splitChar(s: String, c: Char): (String, String) = {
    var max = 0
    var i = s.lastIndexOf(c)
    var chars = s.take(i + 1).toSet
    val visited = mutable.ListBuffer.empty[Char]
    while(chars.nonEmpty || max < i) {
      i = s.lastIndexOf(chars.head)
      visited += chars.head
      if(max < i) {
        max = i
        chars = s.take(max + 1).toSet.filterNot(visited.contains(_))
      } else {
        chars = chars.tail
      }
    }
    (s.take(max + 1), s.takeRight(s.length - max - 1))
  }

  def partitionLabels(s: String): List[Int] = {
    val buf = mutable.ListBuffer.empty[Int]
    var str = s
    while(str.nonEmpty) {
      val (first, second) = splitChar(str, str.head)
      // println(first)
      // println(second)
      buf += first.length
      str = second
    }
    // println(buf)
    buf.toList
  }
}
