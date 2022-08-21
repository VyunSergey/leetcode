package problems.hard

import scala.collection.mutable

object MinimumNumberOfRefuelingStops {
  def main(args: Array[String]): Unit = {
    val Array(target, startFuel) = Console.in.readLine().split("\\s+").take(2).map(_.toInt)
    val stations = Console.in.readLine().split("\\s+").map(_.split(",").take(2).map(_.toInt)).sortBy(_.head)
    assert(stations.forall(_.length == 2))
    assert(stations.forall(_.head < target))
    val res = minRefuelStops(target, startFuel, stations)
    println(res)
  }

  def minRefuelStops(target: Int, startFuel: Int, stations: Array[Array[Int]]): Int = {
    var fuel = startFuel
    var count = 0
    var prev = 0
    val queue = mutable.PriorityQueue.empty[Int]

    for(st <- stations) {
      val loc = st(0)
      val gas = st(1)
      fuel -= loc - prev
      // if get to loc is too far
      // then must refuel in past
      while(queue.nonEmpty && fuel < 0) {
        fuel += queue.dequeue
        count += 1
      }

      if (fuel < 0) return -1
      queue += gas
      prev = loc
    }

    // Repeat body for station:
    // (target, inf)
    fuel -= target - prev
    while(queue.nonEmpty && fuel < 0) {
      fuel += queue.dequeue
      count += 1
    }

    if (fuel < 0) return -1
    count
  }
}
