package problems.easy

import scala.annotation.tailrec

object TwoSum {
  def main(args: Array[String]): Unit = {
    val nums: Array[Int] = Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => x.toIntOption).collect { case Some(x) => x }
    val target = Console.in.readLine.toInt
    println(
      twoSum(nums, target).sliding(2, 2)
        .map { case Array(i, j) =>
          if (i >= 0 && j >= 0) s"i=$i j=$j ${nums(i)}+${nums(j)}=${nums(i) + nums(j)}"
          else s"i=$i j=$j No Solution for target=$target"
        }.mkString("\n")
    )
  }

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val len = nums.length
    val map = nums.zipWithIndex.groupBy(_._1)
    val sorted = nums.sorted

    @tailrec
    def search(left: Int, right: Int, target: Int): Option[Int] = {
      if (left > right) None
      else if (target < sorted(left)) None
      else if (target > sorted(right)) None
      else {
        val mid = (left + right) / 2
        if (target < sorted(mid)) search(left, mid - 1, target)
        else if (target > sorted(mid)) search(mid + 1, right, target)
        else {
          // println(s"l=$left r=$right m=$mid t=$target e=${sorted(mid)}")
          Some(mid)
        }
      }
    }

    sorted.indices.foreach { i =>
      // println(s"i=$i target=$target e=${sorted(i)} t=${target - sorted(i)}")
      search(0, i - 1, target - sorted(i)).map { j =>
        return Array(map(sorted(i)).head._2, map(sorted(j)).last._2)
      }
      search(i + 1, len - 1, target - sorted(i)).map { j =>
        return Array(map(sorted(i)).head._2, map(sorted(j)).last._2)
      }
    }

    Array(-1, -1)
  }
}
