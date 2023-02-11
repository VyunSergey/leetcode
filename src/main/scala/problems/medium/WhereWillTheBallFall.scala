package problems.medium

import scala.collection.mutable

object WhereWillTheBallFall {

  def main(args: Array[String]): Unit = {
    val n = Console.in.readLine().toInt
    val grid: Array[Array[Int]] = Array.fill(n) {
      Console.in.readLine().split(" ").map(_.toIntOption).collect { case Some(i) => i }
    }

    assert(grid.nonEmpty)
    assert(grid.forall(_.length == grid.head.length))
    assert(grid.forall(_.forall(Set(1, -1).contains)))

    val res = findBall(grid)
    println(s"The columns that the balls fall out of at the bottom is ${res.mkString("[", ", ", "]")}")
  }

  def findBall(grid: Array[Array[Int]]): Array[Int] = {
    val m = grid.length
    val n = grid.head.length
    val ans = Array.fill(n)(0)
    val queue = mutable.Queue.empty[Int]
    var row = 0

    for(i <- 0 until n) {
      ans(i) = i
      queue += i
    }

    while(queue.nonEmpty && row < m) {
      val size = queue.size
      // println(s"size=$size")
      for(_ <- 0 until size) {
        val i = queue.dequeue
        val col = ans(i)
        val a = grid(row)(col)
        if (0 <= col + a && col + a < n && grid(row)(col + a) + a != 0) {
          ans(i) = col + a
          queue += i
        } else ans(i) = -1
      }
      // println(ans.mkString("[", "][", "]"))
      row += 1
    }

    ans
  }
}
