package study.array

object MergeSortedArray {
  def main(args: Array[String]): Unit = {
    val nums1: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(i) => i }
    val nums2: Array[Int] = Console.in.readLine().split(" ")
      .map(_.toIntOption).collect { case Some(i) => i }
    println((nums1.toList, nums2.toList))

    val numsAll: Array[Int] = nums1 ++ Array.fill(nums2.length)(0)
    merge(numsAll, nums1.length, nums2, nums2.length)
    println(numsAll.toList)
  }

  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
    var i = m - 1
    var j = n - 1
    var k = n + m - 1

    // [1 3 3 4 5 6 7 9 0 0 0 0]  -> [1 1 2 3 3 3 4 4 5 6 7 9]
    // [1 2 3 4]
    while(i >= 0 && j >= 0 && k >= 0) {
      if (nums1(i) < nums2(j)) {
        nums1(k) = nums2(j)
        j -= 1
      } else {
        nums1(k) = nums1(i)
        i -= 1
      }
      // println((k, i, j, nums1.toList, nums2.toList))
      k -=1
    }

    while(i >= 0 && k >= 0) {
      nums1(k) = nums1(i)
      // println((k, i, j, nums1.toList, nums2.toList))
      i -= 1
      k -= 1
    }

    while(j >= 0 && k >= 0) {
      nums1(k) = nums2(j)
      // println((k, i, j, nums1.toList, nums2.toList))
      j -= 1
      k -= 1
    }
  }
}
