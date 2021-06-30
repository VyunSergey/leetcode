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
}
