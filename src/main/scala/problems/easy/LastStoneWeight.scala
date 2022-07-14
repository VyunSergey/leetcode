package problems.easy

import scala.collection.mutable

object LastStoneWeight {
  def main(args: Array[String]): Unit = {
    val stones: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(x) => x }
    val res: Int = lastStoneWeight(stones)
    println(res)
  }

  def lastStoneWeight(stones: Array[Int]): Int = {
    val queue = mutable.PriorityQueue.empty[Int]

    queue ++= stones
    while(queue.size > 1) {
      val (first, second) = (queue.dequeue, queue.dequeue)
      queue += first - second
      // println(queue)
    }

    queue.dequeue
  }
}
