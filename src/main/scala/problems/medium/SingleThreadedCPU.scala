package problems.medium

import scala.collection.mutable

object SingleThreadedCPU {

  def main(args: Array[String]): Unit = {
    val tasks: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(i) => i }.take(2))

    val res = getOrder(tasks)
    println(s"The order in which the CPU will process the tasks is ${res.mkString("[", ", ", "]")}")
  }

  def getOrder(tasks: Array[Array[Int]]): Array[Int] = {
    val n = tasks.length
    var time = 0
    val sorted = tasks.zipWithIndex.sortBy { case (arr, _) => arr(0) }
    val order: Ordering[Int] = Ordering.by(i => (-sorted(i)._1(1), -sorted(i)._2))
    val queue = mutable.PriorityQueue.empty[Int](order)
    val ans = Array.fill(n)(0)
    var i = 0
    var j = 0

    while(j < n) {
      while(i < n && sorted(i)._1(0) <= time) {
        queue += i
        i += 1
      }
      if (queue.nonEmpty) {
        val k = queue.dequeue
        time = time.max(sorted(k)._1(0)) + sorted(k)._1(1)
        ans(j) = sorted(k)._2
        j += 1
      } else {
        time = time.max(sorted(i)._1(0))
      }
    }

    ans
  }
}
