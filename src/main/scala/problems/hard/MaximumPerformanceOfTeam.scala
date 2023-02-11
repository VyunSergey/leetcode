package problems.hard

import scala.collection.mutable

object MaximumPerformanceOfTeam {
  val modulo: BigInt = BigInt(1000000007)

  def main(args: Array[String]): Unit = {
    val n = Console.in.readLine().toInt
    val k = Console.in.readLine().toInt

    val speed = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(i) => i }.take(6)

    val efficiency = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(i) => i }.take(6)

    val res = maxPerformance(n, speed, efficiency, k)
    println(s"The maximum performance of a team is $res")
  }

  def maxPerformance(n: Int, speed: Array[Int], efficiency: Array[Int], k: Int): Int = {
    // array for storing team efficiency and speed
    val arr = Array.fill(n, 2)(0)

    for(i <- 0 until n) arr(i) = Array(efficiency(i), speed(i))

    // sorted by efficiency descending
    val ess = arr.sortBy(a => -a(0))

    // sorted by speed descending
    val ordering: Ordering[Int] = Ordering.by(x => -x)
    val queue = mutable.PriorityQueue.empty[Int](ordering)

    // total performance of a team = [total speed] * [minimum efficiency]
    var res = BigInt(0)

    // total speed of a team
    var sumS = BigInt(0)

    // for all candidates sorted by efficiency descending
    for(es <- ess) {
      // add speed of the candidate to queue
      queue += es(1)
      // add speed of the candidate to total speed
      sumS += es(1)
      // if it is more then `k` candidates - remove candidate with minimum speed
      if (queue.size > k) sumS -= queue.dequeue
      // recalculate total performance
      res = res.max(sumS * es(0))
    }

    (res % modulo).toInt
  }

}
