package study.array

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MaximumProductSubArrayTest extends AnyFlatSpec with Matchers {

  "maxProduct" should "correctly fin maximum product sub array for test 1" in {
    val nums: Array[Int] = Array(2, 3, -2, 4)
    MaximumProductSubArray.maxProduct(nums) shouldBe 6
  }

  it should "correctly fin maximum product sub array for test 2" in {
    val nums: Array[Int] = Array(4, 6, -2, 9, -2, -5, 7, -10, 3, 4, 6)
    MaximumProductSubArray.maxProduct(nums) shouldBe 21772800
  }

  it should "correctly fin maximum product sub array for test 3" in {
    val nums: Array[Int] = Array(-2, 0, -1)
    MaximumProductSubArray.maxProduct(nums) shouldBe 0
  }

  it should "correctly fin maximum product sub array for test 4" in {
    val nums: Array[Int] = Array(2, 0, 2, 4, 6, -2, 0, 9, -2, -5, 7, 0, -10, 3, 4, 6)
    MaximumProductSubArray.maxProduct(nums) shouldBe 630
  }
}
