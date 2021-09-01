package problems.hard

import scala.collection.mutable

object DungeonGame {
  def main(args: Array[String]): Unit = {
    val dungeon: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) => x })
    assert(dungeon.nonEmpty)
    assert(dungeon.forall(_.length == dungeon.head.length))
    val res = calculateMinimumHP(dungeon)
    println(res)
  }

  def calculateMinimumHP(dungeon: Array[Array[Int]]): Int = {
    if (dungeon.isEmpty) return 1

    val m = dungeon.length
    val n = dungeon.head.length
    val dp = Array.fill(m + 1)(Array.fill(n + 1)(Int.MaxValue))

    dp(m - 1)(n) = 1
    dp(m)(n - 1) = 1

    Range(m - 1, -1, -1).foreach { i =>
      Range(n - 1, -1, -1).foreach { j =>
        dp(i)(j) = Math.max(Math.min(dp(i + 1)(j), dp(i)(j + 1)) - dungeon(i)(j), 1)
      }
    }

    dp(0)(0)
  }

  def calculateMinimumHP2(dungeon: Array[Array[Int]]): Int = {
    if (dungeon.isEmpty) return 1

    val m = dungeon.length
    val n = dungeon.head.length
    implicit val queueOrder: Ordering[((Int, Int), (Int, Int))] =
      Ordering.by[((Int, Int), (Int, Int)), (Int, Int)](_._1)
    val queue = mutable.PriorityQueue.empty[((Int, Int), (Int, Int))]
    val head = dungeon.head.head
    var knightHP = Int.MaxValue

    queue.enqueue(((0, 0), (head, head)))
    while(queue.nonEmpty) {
      val prev = queue.dequeue
      // println(s"(i,j)=${prev._1}, (currHP, minHP)=${prev._2}")
      val ((i, j), (currHP, minHP)) = prev
      if (i == m - 1 && j == n - 1) {
        val hp = Math.max(-1 * minHP, 0) + 1
        knightHP = Math.min(knightHP, hp)
        // println(s"knightHP=$knightHP")
      } else {
        if (i < m - 1) {
          val newCurrHP = currHP + dungeon(i + 1)(j)
          val newMinHP = Math.min(minHP, newCurrHP)
          queue.enqueue(((i + 1, j), (newCurrHP, newMinHP)))
        }
        if (j < n - 1) {
          val newCurrHP = currHP + dungeon(i)(j + 1)
          val newMinHP = Math.min(minHP, newCurrHP)
          queue.enqueue(((i, j + 1), (newCurrHP, newMinHP)))
        }
      }
    }
    knightHP
  }
}
