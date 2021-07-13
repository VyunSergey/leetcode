package problems.medium

import scala.collection.mutable

object KSmallestElementOfSortedMatrix {
  def main(args: Array[String]): Unit = {
    val matrix = Array(
      Array( 5,  8, 10, 12),
      Array( 6, 12, 15, 16),
      Array( 9, 13, 17, 18),
      Array(11, 14, 20, 21)
    )
    val k = Console.in.readLine().toInt
    val res: Int = kthSmallest(matrix, k)
    println(res)
  }

  final case class Pair(x: Int, y: Int, value: Int)

  object Pair {
    implicit val orderingPair: Ordering[Pair] =
      (x: Pair, y: Pair) => y.value - x.value
  }

  def kthSmallest(matrix: Array[Array[Int]], k: Int): Int = {
    val n = matrix.length
    val pq = mutable.PriorityQueue.empty[Pair]

    (0 until n).foreach { j =>
      pq.enqueue(Pair(0, j, matrix(0)(j)))
    }

    (0 until k - 1).foreach { _ =>
      val pair = pq.dequeue()
      // println(pq)
      // println(s"i=$i, a=${pair.value}")
      if (pair.x < n-1) {
        pq.enqueue(Pair(pair.x + 1, pair.y, matrix(pair.x + 1)(pair.y)))
      }
    }

    pq.dequeue().value
  }
}
