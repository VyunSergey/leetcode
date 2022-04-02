package study.array

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MergeSortedArrayTest extends AnyFlatSpec with Matchers {

  "merge" should "correctly merge arrays for test 1" in {
    val nums1: Array[Int] = Array(1, 2, 3)
    val nums2: Array[Int] = Array(2, 5, 6)
    val numsAll: Array[Int] = nums1 ++ Array.fill(nums2.length)(0)
    MergeSortedArray.merge(numsAll, nums1.length, nums2, nums2.length)
    numsAll.sameElements(Array(1, 2, 2, 3, 5, 6)) shouldBe true
  }

  "merge" should "correctly merge arrays for test 2" in {
    val nums1: Array[Int] = Array(1)
    val nums2: Array[Int] = Array.empty[Int]
    val numsAll: Array[Int] = nums1 ++ Array.fill(nums2.length)(0)
    MergeSortedArray.merge(numsAll, nums1.length, nums2, nums2.length)
    numsAll.sameElements(Array(1)) shouldBe true
  }

  "merge" should "correctly merge arrays for test 3" in {
    val nums1: Array[Int] = Array(0)
    val nums2: Array[Int] = Array(1)
    val numsAll: Array[Int] = nums1 ++ Array.fill(nums2.length)(0)
    MergeSortedArray.merge(numsAll, nums1.length, nums2, nums2.length)
    numsAll.sameElements(Array(0, 1)) shouldBe true
  }

  "merge" should "correctly merge arrays for test 4" in {
    val nums1: Array[Int] = Array(1, 3, 3, 4, 5, 6, 7, 9)
    val nums2: Array[Int] = Array(1, 2, 3, 4)
    val numsAll: Array[Int] = nums1 ++ Array.fill(nums2.length)(0)
    MergeSortedArray.merge(numsAll, nums1.length, nums2, nums2.length)
    numsAll.sameElements(Array(1, 1, 2, 3, 3, 3, 4, 4, 5, 6, 7, 9)) shouldBe true
  }
}
