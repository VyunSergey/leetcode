package problems.medium

import scala.collection.Searching._

object ThreeSumClosest {
  def main(args: Array[String]): Unit = {
    val nums = Console.in.readLine.split(" ")
      .filter(_.nonEmpty)
      .map(x => x.toIntOption)
      .collect{ case Some(x) => x }
    val target = Console.in.readLine.toInt
    val res = threeSumClosest(nums, target)
    println(res)
  }

  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    var i = 0
    var j = 0
    var k = 0
    var search = 0
    var sum = 0
    var bestSum = 0
    var dif = Int.MaxValue
    val len = nums.length
    val sorted = nums.sorted

    def searchSlice(i: Int, j: Int, search: Int): Int = {
      sorted.search(search, i, j) match {
        case Found(s) => Math.min(s, Math.max(j - 1, 0))
        case InsertionPoint(s) => Math.min(s, Math.max(j - 1, 0))
      }
    }

    def updateSum(i: Int, j: Int, k: Int): Unit = {
      sum = sorted(i) + sorted(j) + sorted(k)
      if (!List(i, j).contains(k) && Math.abs(sum - target) < dif) {
        bestSum = sum
        dif = Math.abs(bestSum - target)
        // println(s"i=$i j=$j k=$k sum=$bestSum dif=$dif sorted=${sorted.toList}")
      }
    }

    while(i < len - 2) {
      j = i + 1
      while(j < len - 1) {
        search = target - (sorted(i) + sorted(j))

        // search in [0;i)
        if (sorted(0) < search && search < sorted(Math.max(i - 1, 0))) {
          k = searchSlice(0, i, search)
          updateSum(i, j, k)
        } else {
          updateSum(i, j, 0)
          updateSum(i, j, Math.max(i - 1, 0))
        }
        if (dif == 0) return bestSum

        // search in [i+1;j)
        if (sorted(Math.min(i + 1, len - 1)) < search && search < sorted(Math.max(j - 1, 0))) {
          k = searchSlice(i + 1, j, search)
          updateSum(i, j, k)
        } else {
          updateSum(i, j, Math.min(i + 1, len - 1))
          updateSum(i, j, Math.max(j - 1, 0))
        }
        if (dif == 0) return bestSum

        // search in [j+1;len)
        if (sorted(Math.min(j + 1, len - 1)) < search && search < sorted(len - 1)) {
          k = searchSlice(j + 1, len, search)
          updateSum(i, j, k)
        } else {
          updateSum(i, j, Math.min(j + 1, len - 1))
          updateSum(i, j, len - 1)
        }
        if (dif == 0) return bestSum
        j += 1
      }
      i += 1
    }

    bestSum
  }
}
