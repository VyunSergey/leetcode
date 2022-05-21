package problems.medium

import scala.collection.mutable

object MinimumNumberOfVerticesReachAllNodes {
  def main(args: Array[String]): Unit = {
    val n = Console.in.readLine().toInt
    val edges: List[List[Int]] = Console.in.readLine().split(" ").map(_.split(",").map(_.toInt).toList).toList
    val res = findSmallestSetOfVertices(n, edges)
    println(res)
  }

  def findSmallestSetOfVertices(n: Int, edges: List[List[Int]]): List[Int] = {
    val res = mutable.ListBuffer.empty[Int]

    (0 until n).foreach { i =>
      if (!edges.exists(_.last == i)) {
        res += i
      }
    }

    res.result
  }
}
