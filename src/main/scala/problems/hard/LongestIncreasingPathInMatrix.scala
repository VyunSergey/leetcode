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
    if (matrix.isEmpty) return 0

    val n = matrix.length
    val m = matrix.head.length
    var maxLen = 0

    implicit val queueOrdering: Ordering[((Int, Int), Int)] =
      Ordering.by[((Int, Int), Int), Int] { case (_, x) => x }
    val queue = mutable.PriorityQueue.empty[((Int, Int), Int)]

    (0 until n).foreach { i =>
      (0 until m).foreach { j =>
        queue.enqueue(((i, j), matrix(i)(j)))
      }
    }

    val dp = Array.fill(n)(new Array[Int](m))
    val directions = Array((-1, 0), (1, 0), (0, -1), (0, 1))

    while (queue.nonEmpty) {
      // println(s"queue size=${queue.size}")
      val ((i, j), _) = queue.dequeue
      // println(s"point=${(i, j)} maxLen=$maxLen")
      dp(i)(j) = 1
      directions.map { case (dx, dy) => (i + dx, j + dy) }
        .filter { case (k, l) =>
          (0 <= k && k < n) &&
            (0 <= l && l < m) &&
            matrix(i)(j) < matrix(k)(l)
        }
        .foreach { case (k, l) =>
          dp(i)(j) = Math.max(dp(i)(j), dp(k)(l) + 1)
        }
      maxLen = Math.max(maxLen, dp(i)(j))
    }
    maxLen
  }
}
