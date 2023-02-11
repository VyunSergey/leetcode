package problems.medium

import scala.annotation.tailrec

object FindClosestNodeToGivenTwoNodes {

  def main(args: Array[String]): Unit = {
    val edges: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(i) => i }

    val node1: Int = Console.in.readLine().toInt
    val node2: Int = Console.in.readLine().toInt

    val res: Int = closestMeetingNode(edges, node1, node2)
    println(s"closest meeting node is $res")
  }

  @tailrec
  def path(i: Int, n: Int, moves: Int, reached: Array[Int], edges: Array[Int]): Unit = {
    if (0 <= i && i <= n - 1) {
      reached(i) = moves
      if (edges(i) >= 0 && reached(edges(i)) < 0) path(edges(i), n, moves + 1, reached, edges)
    }
  }

  def closestMeetingNode(edges: Array[Int], node1: Int, node2: Int): Int = {
    val n = edges.length
    val reach1 = Array.fill(n)(-1)
    val reach2 = Array.fill(n)(-1)

    path(node1, n, moves = 0, reached = reach1, edges = edges)
    path(node2, n, moves = 0, reached = reach2, edges = edges)

    var ans = -1
    var minDist = Int.MaxValue

    for(i <- 0 until n if reach1(i) > -1 && reach2(i) > -1 && Math.max(reach1(i), reach2(i)) < minDist) {
      minDist = Math.max(reach1(i), reach2(i))
      ans = i
    }

    ans
  }
}
