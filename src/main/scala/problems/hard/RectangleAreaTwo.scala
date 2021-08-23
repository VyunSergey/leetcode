package problems.hard

import scala.collection.mutable

object RectangleAreaTwo {
  /* Segment Tree data structure
   **/
  class Node(val start: Int, val end: Int, val X: Array[Int],
             var left: Node = null, var right: Node = null,
             var count: Int = 0, var total: Long = 0L) {
    def getRangeMid: Int = start + (end - start) / 2

    def getLeft: Node = {
      if (left != null) left else {
        left = new Node(start, getRangeMid, X)
        left
      }
    }

    def getRight: Node = {
      if (right != null) right else {
        right = new Node(getRangeMid, end, X)
        right
      }
    }

    def update(i: Int, j: Int, value: Int): Long = {
      if (i >= j) return 0L

      if (start == i && end == j) {
        count += value
      } else {
        getLeft.update(i, Math.min(getRangeMid, j), value)
        getRight.update(Math.max(getRangeMid, i), j, value)
      }

      if (count > 0) total = X(end) - X(start)
      else total = getLeft.total + getRight.total

      total
    }
  }

  def main(args: Array[String]): Unit = {
    val rectangles: Array[Array[Int]] = Console.in.readLine().split(" ")
      .map(_.split(",").map(_.toIntOption).collect { case Some(a) => a }.take(4))
    val res = rectangleArea(rectangles)
    println(res)
  }

  def rectangleArea(rectangles: Array[Array[Int]]): Int = {
    val OPEN = 1
    val CLOSE = -1
    val events = Array.fill[Array[Int]](2 * rectangles.length)(Array.empty[Int])
    val Xvals = mutable.HashSet.empty[Int]
    var i = 0

    rectangles.foreach { rect =>
      val Array(x0, y0, x1, y1) = rect
      if (x0 < x1 && y0 < y1) {
        events(i) = Array(y0, OPEN, x0, x1)
        i += 1
        events(i) = Array(y1, CLOSE, x0, x1)
        i += 1
        Xvals ++= Array(x0, x1)
      }
    }

    val sortedEvents = events.sortBy(_.headOption.getOrElse(Int.MaxValue))
    val sortedX = Xvals.toArray.sorted
    val Xi = mutable.HashMap.empty[Int, Int]

    sortedX.indices.foreach { i =>
      Xi += (sortedX(i) -> i)
    }

    val tree = new Node(0, sortedX.length - 1, sortedX)
    var result = 0L
    var currentSumX = 0L
    var currentY = sortedEvents(0)(0)

    sortedEvents.takeWhile(_.nonEmpty).foreach { event =>
      val Array(y, eventType, x0, x1) = event
      result += currentSumX * (y - currentY)
      currentSumX = tree.update(Xi(x0), Xi(x1), eventType)
      currentY = y
    }

    (result % (1e9.toInt + 7)).toInt
  }
}
