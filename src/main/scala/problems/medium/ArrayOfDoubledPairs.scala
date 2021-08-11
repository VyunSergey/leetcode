package problems.medium

import scala.collection.mutable

object ArrayOfDoubledPairs {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Console.in.readLine().split(" ").map(_.toIntOption)
      .collect { case Some(x) => x }
    val res = canReorderDoubled(arr)
    println(res)
  }

  def canReorderDoubled(arr: Array[Int]): Boolean = {
    if (arr.sum % 3 != 0) return false
    if (arr.isEmpty) return true

    val sorted = arr.sortBy(x => Math.abs(x))
    val map = mutable.HashMap.empty[Int, Int]

    sorted.foreach { x =>
      map += (x -> (map.getOrElse(x, 0) + 1))
    }

    sorted.foreach { x =>
      // check only if x is present in map
      if (map.getOrElse(x, 0) > 0) {
        // check if 2*x is present in map
        if (map.getOrElse(2 * x, 0) <= 0) return false
        // update map removing x and 2*x
        map += (x -> (map.getOrElse(x, 0) - 1))
        map += (2 * x -> (map.getOrElse(2 * x, 0) - 1))
      }
    }
    true
  }

}
