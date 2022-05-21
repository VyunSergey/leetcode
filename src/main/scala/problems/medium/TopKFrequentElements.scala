package problems.medium

import scala.collection.mutable

object TopKFrequentElements {
  def main(args: Array[String]): Unit = {
    val nums = Console.in.readLine().split(" ").map(_.toInt)
    val k = Console.in.readLine().toInt
    val res = topKFrequent(nums, k)
    println(res.toList)
  }

  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
    var i = 0
    val top = mutable.ListBuffer.empty[Int]
    val grp = nums.groupMapReduce(identity)(_ => 1)(_ + _)
    // println(grp)
    while (i < k) {
      top += grp.view.filterKeys(!top.contains(_))
        .toList
        .maxBy(_._2)
        ._1
      i += 1
    }
    top.toArray
  }

  def topKFrequentTwo(nums: Array[Int], k: Int): Array[Int] = {
    val res = mutable.ArrayBuffer.empty[Int]
    val map = nums.groupMapReduce(identity)(_ => 1)(_ + _)
    val ordering: Ordering[Int] = Ordering.by(n => map(n))
    val queue = mutable.PriorityQueue.from(nums.toSet)(ordering)

    (0 until k).foreach { _ =>
      res += queue.dequeue
    }
    res.toArray
  }
}
