package problems.medium

import scala.collection.mutable

object KeysAndRooms {
  def main(args: Array[String]): Unit = {
    val rooms: List[List[Int]] = Console.in.readLine().split(" ").map(_.split(",").map(_.toInt).toList).toList
    val res = canVisitAllRooms(rooms)
    println(res)
  }

  def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
    val n = rooms.length
    val queue = mutable.Queue.empty[Int]
    val used = Array.fill(n)(0)
    used(0) = 1

    queue.enqueue(0)
    while(queue.nonEmpty) {
      val key = queue.dequeue()
      for {
        newKey <- rooms(key) if used(newKey) == 0
      } {
        used(newKey) = 1
        queue.enqueue(newKey)
      }
    }

    used.sum == n
  }
}
