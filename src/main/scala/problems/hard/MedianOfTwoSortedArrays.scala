package problems.hard

object MedianOfTwoSortedArrays {
  def main(args: Array[String]): Unit = {
    val nums1: Array[Int] = Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}
    val nums2: Array[Int] = Console.in.readLine.split(" ")
      .filter(_.nonEmpty).map(x => scala.util.Try(x.toInt).toOption).collect{case Some(x) => x}
    Console.out.println(findMedianSortedArrays(nums1, nums2))
  }

  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val I = nums1.length
    val J = nums2.length
    val N = (I + J) / 2 + (I + J) % 2

    var n = 0
    var i = 0
    var j = 0
    var med =
      if (I > 0 && J > 0) Math.min(nums1.head, nums2.head)
      else if (I > 0) nums1.head
      else if (J > 0) nums2.head
      else 0
    var medPrev = med

    while (n <= N) {
      if (I > i && J > j) {
        if (nums1(i) <= nums2(j)) {
          medPrev = med
          med = nums1(i)
          i = i + 1
        } else {
          medPrev = med
          med = nums2(j)
          j = j + 1
        }
      } else if (I > i) {
        medPrev = med
        med = nums1(i)
        i = i + 1
      } else if (J > j) {
        medPrev = med
        med = nums2(j)
        j = j + 1
      } else ()
      n = n + 1
    }

    if ((I + J) % 2 == 0) (medPrev.toDouble + med.toDouble) / 2.0
    else medPrev.toDouble
  }
}
