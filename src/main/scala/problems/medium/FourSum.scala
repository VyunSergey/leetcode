package problems.medium

import scala.collection.mutable

object FourSum {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine().split(" ").map(_.toInt)
    val target: Int = Console.in.readLine().toInt
    val res: List[List[Int]] = fourSum(nums, target)
    println(res)
  }

  val bfs = mutable.HashMap.empty[Int, mutable.HashSet[List[Int]]]

  (1 to 3).foreach { i =>
    bfs += (i -> mutable.HashSet.empty[List[Int]])
  }

  def fourSum(nums: Array[Int],
              target: Int): List[List[Int]] = {
    // println(nums.toList)
    val len = nums.length
    val numsS = nums.sorted.toList
    // println(numsS.toList)
    if (len < 4) return List.empty[List[Int]]
    if (numsS.take(4).sum > target || numsS.takeRight(4).sum < target) {
      return List.empty[List[Int]]
    }
    var i = 0
    bfs(3).clear()
    while(i < len - 3) {
      // println(s"MAIN i=$i a=${numsS(i)}")
      search3(numsS, i + 1, len, target - numsS(i)).foreach { lst =>
        bfs(3) += numsS(i) :: lst
      }
      i += 1
    }
    bfs(3).toList
  }

  def search3(nums: List[Int], from: Int, until: Int, target: Int): List[List[Int]] = {
    // println(s"IN s3 f=$from u=$until t=$target")
    if (nums.slice(from, from + 3).sum > target || nums.slice(until - 3, until).sum < target) {
      return List.empty[List[Int]]
    }
    var i = from
    bfs(2).clear()
    while(i < until - 2) {
      // println(s"s3 i=$i a2=${nums(i)}")
      search2(nums, i + 1, until, target - nums(i)).foreach { lst =>
        bfs(2) += nums(i) :: lst
      }
      i += 1
    }
    bfs(2).toList
  }

  def search2(nums: List[Int], from: Int, until: Int, target: Int): List[List[Int]] = {
    // println(s"IN s2 f=$from u=$until t=$target")
    if (nums.slice(from, from + 2).sum > target || nums.slice(until - 2, until).sum < target) {
      return List.empty[List[Int]]
    }
    var i = from
    bfs(1).clear()
    while(i < until - 1) {
      // println(s"s2 i=$i a3=${nums(i)}")
      search(nums, i + 1, until, target - nums(i)).foreach { a =>
        bfs(1) += nums(i) :: List(a)
      }
      i += 1
    }
    bfs(1).toList
  }

  // return the first element in a slice of a sorted list that equals to target
  def search(nums: List[Int], from: Int, until: Int, target: Int): List[Int] = {
    // println(s"IN s, f=$from u=$until t=$target")
    if (nums(from) > target || nums(until - 1) < target) {
      return List.empty[Int]
    }
    var i = from
    while(i < until && nums(i) < target) i += 1
    if (i < until && nums(i) == target) {
      // println(s"s, i=$i a4=${nums(i)}")
      return List(target)
    }
    List.empty[Int]
  }
}
