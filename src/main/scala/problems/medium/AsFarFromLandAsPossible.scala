package problems.medium

import problems.common.Timed.timed

import scala.collection.mutable

object AsFarFromLandAsPossible {
  def main(args: Array[String]): Unit = {
    val grid: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) => x })
    printArr(grid)

    val res = maxDistance(grid)
    val resQ = maxDistanceQueue(grid)
    println(timed(res))
    println(timed(resQ))
  }

  def printArr(arr: Array[Array[Int]]): Unit = {
    println(arr.map(_.mkString("[", "],[", "]")).mkString("\n"))
    println()
  }

  def maxDistanceQueue(grid: Array[Array[Int]]): Int = {
    val m = grid.length
    val n = grid.head.length
    val ordering: Ordering[(Int, Int, Int)] = Ordering.by(-_._3)
    val queue = mutable.PriorityQueue.empty[(Int, Int, Int)](ordering)
    var maxDist = -1
    // println(s"m=$m n=$n")

    for {
      i <- 0 until m
      j <- 0 until n if grid(i)(j) == 1
    } {
      queue += ((i, j, 0))
    }

    while(queue.nonEmpty) {
      val (i, j, dist) = queue.dequeue()

      if (grid(i)(j) == 0) {
        grid(i)(j) = 1
        maxDist = maxDist.max(dist)
      }

      if (0 < i     && grid(i - 1)(j) == 0) queue += ((i - 1, j, dist + 1))
      if (i < m - 1 && grid(i + 1)(j) == 0) queue += ((i + 1, j, dist + 1))
      if (0 < j     && grid(i)(j - 1) == 0) queue += ((i, j - 1, dist + 1))
      if (j < n - 1 && grid(i)(j + 1) == 0) queue += ((i, j + 1, dist + 1))
    }

    maxDist
  }

  def maxDistance(grid: Array[Array[Int]]): Int = {
    val MAX = 10000
    val m = grid.length
    val n = grid.head.length
    val dist = Array.fill(m)(Array.fill(n)(MAX))
    var maxDist = -1
    // println(s"m=$m n=$n")

    // first
    dist(0)(0) = if (grid(0)(0) == 1) 0 else dist(0)(0)

    // last
    dist(m - 1)(n - 1) = if (grid(m - 1)(n - 1) == 1) 0 else dist(m - 1)(n - 1)

    /* #==============#
     * | right + down |
     * #==============#
     */

    // right
    (1 until m).foreach(i => dist(i)(0) = if (grid(i)(0) == 1) 0 else Seq(dist(i - 1)(0) + 1, dist(i)(0)).min)

    // down
    (1 until n).foreach(j => dist(0)(j) = if (grid(0)(j) == 1) 0 else Seq(dist(0)(j - 1) + 1, dist(0)(j)).min)

    // right + down
    for {
      i <- 1 until m
      j <- 1 until n
    } {
      dist(i)(j) = if (grid(i)(j) == 1) 0 else Seq(dist(i - 1)(j) + 1, dist(i)(j - 1) + 1, dist(i)(j)).min
    }
    // printArr(dist)

    /* #==============#
     * |  left + up   |
     * #==============#
     */

    // left
    (m - 2 to 0 by -1).foreach(i => dist(i)(n - 1) = if (grid(i)(n - 1) == 1) 0 else Seq(dist(i + 1)(n - 1) + 1, dist(i)(n - 1)).min)

    // up
    (n - 2 to 0 by -1).foreach(j => dist(m - 1)(j) = if (grid(m - 1)(j) == 1) 0 else Seq(dist(m - 1)(j + 1) + 1, dist(m - 1)(j)).min)

    // left + up
    for {
      i <- m - 2 to 0 by -1
      j <- n - 2 to 0 by -1
    } {
      dist(i)(j) = if (grid(i)(j) == 1) 0 else Seq(dist(i + 1)(j) + 1, dist(i)(j + 1) + 1, dist(i)(j)).min
    }
    // printArr(dist)

    for {
      i <- 0 until m
      j <- 0 until n
    } {
      maxDist = maxDist.max(dist(i)(j))
    }

    if (0 < maxDist && maxDist < MAX) maxDist else -1
  }
}
