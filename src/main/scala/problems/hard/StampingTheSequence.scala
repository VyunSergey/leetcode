package problems.hard

import scala.collection.mutable

object StampingTheSequence {
  def main(args: Array[String]): Unit = {
    val Array(stamp, target) = Console.in.readLine().split("\\s+").take(2)
    val res = movesToStamp(stamp, target)
    println(res.mkString("[", ",", "]"))
  }

  def movesToStamp(stamp: String, target: String): Array[Int] = {
    val targetArr = target.toCharArray
    val n = target.length
    val m = stamp.length
    val ans = mutable.ArrayBuffer.empty[Int]
    val seen = mutable.HashSet.empty[Int]

    def equals(i: Int): Boolean = {
      for (j <- 0 until m) {
        if (targetArr(i + j) != stamp(j) && targetArr(i + j) != '?') return false
      }
      true
    }

    for (i <- 0 to n - m) {
      // println(s"i=$i targetArr=${targetArr.mkString}")
      if (equals(i)) {
        for (x <- i to 0 by -1 if !seen.contains(x)) {
          seen += x
          if (equals(x)) {
            ans += x
            // println(s"x=$x ?-[$x, ${x+m-1}] ans=${ans.mkString}")
            for (j <- 0 until m) targetArr(x + j) = '?'
          }
        }
      }
    }

    if (targetArr.forall(c => c == '?')) ans.toArray.reverse else Array.empty[Int]
  }

  def movesToStampPQ(stamp: String, target: String): Array[Int] = {
    val n = target.length
    val m = stamp.length
    val chars = stamp.toSet
    if (target(0) != stamp(0) || target(n - 1) != stamp(m - 1)) return Array.empty[Int]
    if (target.exists(c => !chars.contains(c))) return Array.empty[Int]

    def diff(lst: List[Int]): Int = {
      var count = n
      val visited = Array.fill(n)(false)
      // val sb = new mutable.StringBuilder
      for {
        i <- lst
        j <- i + m - 1 to i by -1 if !visited(j)
      } {
        // sb += stamp(j-i)
        visited(j) = true
        if (stamp(j - i) == target(j)) {
          count -= 1
        }
      }
      // println(sb.result.reverse)
      count
    }

    val ordering: Ordering[List[Int]] = Ordering.by(lst => -diff(lst))
    val queue = mutable.PriorityQueue.empty[List[Int]](ordering)

    queue += List.empty[Int]
    while(queue.nonEmpty) {
      val lst = queue.dequeue
      val df = diff(lst)
      // println(lst, diff(lst))
      if (df == 0) return lst.reverse.toArray
      for {
        j <- 0 to n - m if !lst.contains(j) && diff(j :: lst) < df
      } {
        queue += j :: lst
      }
    }

    Array.empty[Int]
  }
}
