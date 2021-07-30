package problems.hard

object FindMedianFromDataStream {
  def main(args: Array[String]): Unit = {
    val medianFinder = new MedianFinder()
    medianFinder.addNum(1)
    println(medianFinder.findMedian())
    medianFinder.addNum(2)
    println(medianFinder.findMedian())
    medianFinder.addNum(0)
    println(medianFinder.findMedian())
    medianFinder.addNum(1)
    medianFinder.addNum(1)
    println(medianFinder.findMedian())
    medianFinder.addNum(3)
    println(medianFinder.findMedian())
    medianFinder.addNum(3)
    println(medianFinder.findMedian())
    medianFinder.addNum(3)
    println(medianFinder.findMedian())
    medianFinder.addNum(3)
    println(medianFinder.findMedian())
    medianFinder.addNum(3)
    println(medianFinder.findMedian())
    medianFinder.addNum(3)
    println(medianFinder.findMedian())
  }

  class MedianFinder() {
    import scala.collection.mutable

    /** initialize your data structure here. */

    // always n/2 elements
    // n=2k => k elements
    // n=2k+1 => k elements
    private var leftSize = 0
    private val left: mutable.TreeMap[Int, Int] = mutable.TreeMap.empty[Int, Int]

    // n/2 or n/2+1 elements
    // n=2k => k elements
    // n=2k+1 => k+1 elements
    private var rightSize = 0
    private val right: mutable.TreeMap[Int, Int] = mutable.TreeMap.empty[Int, Int]

    def addNum(num: Int): Unit = {
      var tmp = 0

      left += (num -> (left.getOrElse(num, 0) + 1))
      tmp = left.lastKey
      right += (tmp -> (right.getOrElse(tmp, 0) + 1))
      if (left(tmp) > 1) {
        left += (tmp -> (left(tmp) - 1))
      } else {
        left.remove(tmp)
      }
      // current n=2k+1 => add to left
      if (leftSize < rightSize) {
        tmp = right.firstKey
        left += (tmp -> (left.getOrElse(tmp, 0) + 1))
        if (right(tmp) > 1) {
          right += (tmp -> (right(tmp) - 1))
        } else {
          right.remove(tmp)
        }
        leftSize += 1
        // current n=2k => add to right
      } else {
        rightSize += 1
      }
    }

    def findMedian(): Double = {
      // current n=2k+1 => right.head
      if (leftSize < rightSize) {
        right.firstKey.toDouble
        // current n=2k => (left.last + right.head)/2
      } else {
        (left.lastKey.toDouble + right.firstKey.toDouble) / 2.0
      }
    }

  }
}
