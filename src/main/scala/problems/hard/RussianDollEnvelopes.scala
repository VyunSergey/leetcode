package problems.hard

import scala.collection.mutable

object RussianDollEnvelopes {
  def main(args: Array[String]): Unit = {
    val envelopes: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(x) => x })
    assert(envelopes.nonEmpty)
    assert(envelopes.forall(_.length == 2))
    assert(envelopes.forall(_.forall(x => 1 <= x && x <= 10000)))
    val res = maxEnvelopes(envelopes)
    println(res)
  }

  def maxEnvelopes(envelopes: Array[Array[Int]]): Int = {
    var count = 0
    val map = mutable.HashMap.empty[(Int, Int), Int]

    envelopes.map { case Array(a, b) =>
      (a, b)
    }.sorted.foreach { case (a, b) =>
      map.filter { case ((c, d), _) =>
        c < a && d < b
      }.maxByOption(_._2).map { case (_, cnt) =>
        map += (a, b) -> (cnt + 1)
        count = Math.max(count, cnt + 1)
      }.getOrElse {
        map += (a, b) -> 1
        count = Math.max(count, 1)
      }
      // println((count, map))
    }
    count
  }
}
