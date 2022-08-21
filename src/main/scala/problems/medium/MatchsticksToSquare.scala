package problems.medium

import scala.collection.mutable

object MatchsticksToSquare {
  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Console.in.readLine.split(" ")
      .map(_.toIntOption).collect { case Some(x) => x }

    val res = makeSquare(arr)
    println(res)
  }

  def makeSquare(matchsticks: Array[Int]): Boolean = {
    val N = 100000L

    val n = matchsticks.length
    if (n < 4) return false

    val perimeter = matchsticks.sum
    if (perimeter % 4 != 0) return false

    val side = perimeter / 4
    val (min, max) = (matchsticks.filter(_ > 0).min, matchsticks.max)
    if (max > side || (max < side && max + min > side)) return false
    if (max == min) return (n - matchsticks.count(_ == 0)) % 4 == 0 && ((n - matchsticks.count(_ == 0)) / 4) * max == side
    println(s"n=$n perimeter=$perimeter side=$side")

    // l = m4*N^3 + m3*N^2 + m2*N + m1
    def fromMasks(m1: Short, m2: Short, m3: Short, m4: Short): Long = {
      List(m1, m2, m3, m4).sorted.foldLeft(0L) { case (acc, m) =>
        acc * N + m
      }
    }

    // l = m4*N^3 + m3*N^2 + m2*N + m1
    // m1 = l % N, l1 = m4*N^2 + m3*N + m2
    // m2 = l1 % N, l2 = m4*N + m3
    // m3 = l2 % N, l3 = m4
    // m4 = l3
    def toMasks(l: Long): (Short, Short, Short, Short) = {
      val m1 = (l % N).toShort
      val l1 = (l - m1) / N
      val m2 = (l1 % N).toShort
      val l2 = (l1 - m2) / N
      val m3 = (l2 % N).toShort
      val m4 = ((l2 - m3) / N).toShort
      (m1, m2, m3, m4)
    }

    val nums = matchsticks.sortBy(x => -x)

    def maskSum(m: Short): Int = {
      var sum = 0
      var i = 0
      while(i < n) {
        if ((m & (1 << i)) != 0) sum += nums(i)
        i += 1
      }
      sum
    }

    val queue = mutable.Queue.empty[Long]
    var k = 0

    queue += 0L
    while(k < n) {
      val a = nums(k)
      val size = queue.size
      println(s"k=$k a=$a size=$size")
      var i = 0
      while(i < size) {
        val (m1, m2, m3, m4) = toMasks(queue.dequeue)
        // println((m1, m2, m3, m4, m1.toBinaryString, m2.toBinaryString, m3.toBinaryString, m4.toBinaryString))
        val (s1, s2, s3, s4) = (maskSum(m1), maskSum(m2), maskSum(m3), maskSum(m4))
        // if (s1 + s2 + s3 + s4 == perimeter - a) println((s1, s2, s3, s4, s"a=$a"))
        if (
          (s1 + a == side && s2 == s3 && s3 == s4 && s4 == side) ||
            (s2 + a == side && s1 == s3 && s3 == s4 && s4 == side) ||
            (s3 + a == side && s1 == s2 && s2 == s4 && s4 == side) ||
            (s4 + a == side && s1 == s2 && s2 == s3 && s3 == side)
        ) {
          // println((s1, s2, s3, s4))
          return true
        }

        val add = (1 << k).toShort
        if (s1 + a <= side) queue += fromMasks((m1 + add).toShort, m2, m3, m4)
        if (s2 + a <= side) queue += fromMasks(m1, (m2 + add).toShort, m3, m4)
        if (s3 + a <= side) queue += fromMasks(m1, m2, (m3 + add).toShort, m4)
        if (s4 + a <= side) queue += fromMasks(m1, m2, m3, (m4 + add).toShort)
        i += 1
      }
      k += 1
    }

    false
  }
}
