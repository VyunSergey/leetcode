package problems.hard

import scala.collection.mutable
import RangeModule.IntervalUtils._

class RangeModule() {
  val ranges = mutable.HashMap.empty[(Int, Int), Boolean]

  def addRange(left: Int, right: Int): Unit = {
    val interval = (left, right)
    // println((interval, "add before", ranges))
    // containing case: exit
    ranges.find { case (range, _) =>
      range.contains(interval)
    }.foreach { _ =>
      return
    }
    // contains case: remove
    ranges.filter { case (range, _) =>
      interval.contains(range)
    }.foreach { case (range, _) =>
      ranges -= range
    }
    // intersects or touches case: union
    ranges += (ranges.filter { case (range, _) =>
      interval.intersects(range) ||
        interval.touches(range)
    }.foldLeft(interval) { case (accInterval, (range, _)) =>
      ranges -= range
      accInterval.union(range)
    } -> true)
    // println((interval, "add after", ranges))
  }

  def queryRange(left: Int, right: Int): Boolean = {
    val interval = (left, right)
    // println((interval, "query", ranges))
    // contains case: find
    ranges.find { case (range, _) =>
      range.contains(interval)
    }.exists(_._2)
  }

  def removeRange(left: Int, right: Int): Unit = {
    val interval = (left, right)
    // println((interval, "rm before", ranges))
    // containing case: remove
    ranges.filter { case (range, _) =>
      interval.contains(range)
    }.foreach { case (range, _) =>
      ranges -= range
    }
    // contains or intersects case: complement
    ranges.filter { case (range, _) =>
      range.contains(interval) ||
        range.intersects(interval)
    }.foreach { case (range, _) =>
      ranges -= range
      range.complement(interval).foreach { range =>
        ranges += (range -> true)
      }
    }
    // println((interval, "rm after", ranges))
  }
}

object RangeModule {

  def main(args: Array[String]): Unit = {
    val commandKeys = List("addRange", "removeRange", "queryRange")
    val tuplePattern = "(\\d+),(\\d+)".r

    val commands: Array[String] = Console.in.readLine().split(" ")
      .collect { case x if commandKeys.contains(x) => x }
    val ranges: Array[(Int, Int)] = Console.in.readLine().split(" ")
      .collect { case tuplePattern(left, right) => (left.toInt, right.toInt) }
    assert(commands.length == ranges.length)

    val rangeModule = new RangeModule()
    commands.zip(ranges).foreach {
      case ("addRange", (left, right)) =>
        rangeModule.addRange(left, right)
      case ("removeRange", (left, right)) =>
        rangeModule.removeRange(left, right)
      case ("queryRange", (left, right)) =>
        println(((left, right), rangeModule.queryRange(left, right)))
      case x => throw new IllegalArgumentException(s"Wrong input $x")
    }
  }

  object IntervalUtils {
    implicit class IntervalOps(val interval: (Int, Int)) extends AnyVal {
      def contains(other: (Int, Int)): Boolean =
        (interval._1 <= other._1) && (other._2 <= interval._2)
      def intersects(other: (Int, Int)): Boolean =
        (interval._1 < other._2) && (other._1 < interval._2)
      def touches(other: (Int, Int)): Boolean =
        (interval._1 == other._2) || (other._1 == interval._2)

      def union(other: (Int, Int)): (Int, Int) =
        (Math.min(interval._1, other._1), Math.max(interval._2, other._2))
      def complement(other: (Int, Int)): Seq[(Int, Int)] =
        if (contains(other) || intersects(other))
          (if (interval._1 < other._1) Seq((interval._1, other._1)) else Seq()) ++
            (if (other._2 < interval._2) Seq((other._2, interval._2)) else Seq())
        else Seq(interval)
    }
  }
}
