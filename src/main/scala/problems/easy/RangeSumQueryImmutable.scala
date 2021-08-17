package problems.easy

import scala.collection.immutable.TreeMap

object RangeSumQueryImmutable {

  class NumArray(_nums: Array[Int]) {

    val cache: TreeMap[Int, Int] = TreeMap.from(
      _nums.indices.map(i => (i, _nums.slice(0, i + 1).sum))
    )

    def sumRange(left: Int, right: Int): Int = {
      cache.getOrElse(right, 0) - cache.getOrElse(left - 1, 0)
    }

  }

  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(x) => x }
    val numArray = new NumArray(nums)
    nums.indices.foreach { i =>
      nums.indices.filter(_ > i).foreach { j =>
        println(s"i=$i j=$j sum=${numArray.sumRange(i, j)}")
      }
    }
  }

}
