package problems.hard

import scala.collection.mutable

object LongestIncreasingPathInMatrix {
  def main(args: Array[String]): Unit = {
    val matrix: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toInt))
    assert(matrix.nonEmpty)
    assert(matrix.forall(_.length == matrix.head.length))
    val res = longestIncreasingPath(matrix)
    println(res)
  }

  def longestIncreasingPath(matrix: Array[Array[Int]]): Int = {
    val m = matrix.length
    val n = matrix.head.length
    val directions = List((-1, 0), (1, 0), (0, -1), (0, 1))
    implicit val ordering: Ordering[(Int, (Int, Int))] = Ordering.by(_._1)
    val queue = mutable.PriorityQueue.empty[(Int, (Int, Int))]
    val dp = Array.fill(m)(Array.fill(n)(0))
    var maxLen = 0

    (0 until m).foreach { i =>
      (0 until n).foreach { j =>
        queue.enqueue((matrix(i)(j), (i, j)))
      }
    }

    def next(x: Int, y: Int): List[(Int, Int)] = {
      for {
        (dx, dy) <- directions if
          (0 <= x + dx) && (x + dx < m) &&
          (0 <= y + dy) && (y + dy < n) &&
          (matrix(x)(y) < matrix(x + dx)(y + dy))
      } yield {
        (x + dx, y + dy)
      }
    }

    while(queue.nonEmpty) {
      // println(s"size=${queue.size} max=$max")
      val (_, (i, j)) = queue.dequeue
      dp(i)(j) = 1
      next(i, j).foreach { case (k, l) =>
        dp(i)(j) = Math.max(dp(i)(j), dp(k)(l) + 1)
      }
      maxLen = Math.max(dp(i)(j), maxLen)
    }

    maxLen
  }
}
