package problems.medium

import scala.collection.mutable

object TopKFrequentWords {
  def main(args: Array[String]): Unit = {
    val words: Array[String] = Console.in.readLine().split(" ")
    val k: Int = Console.in.readLine().toInt
    assert(0 < k && k <= words.toSet.size)
    val res: List[String] = topKFrequent(words, k)
    println(res)
  }

  def topKFrequent(words: Array[String], k: Int): List[String] = {
    val freq: Map[String, Int] = words.groupMapReduce(x => x)(_ => 1)(_ + _)
    val ordering: Ordering[String] = Ordering.by[String, (Int, String)](s => (-freq(s), s)).reverse
    val queue = mutable.PriorityQueue.empty[String](ordering)

    queue ++= freq.keySet
    (0 until k).foldLeft(List.empty[String]) { case (acc, _) =>
      queue.dequeue :: acc
    }.reverse
  }
}
